use peekmore::PeekMore;

#[derive(Debug, PartialEq)]
pub enum Token<'a> {
    INT(i16),
    Name(&'a str),
    CString(&'a str),
    CChar(char),
    KAnd,
    KBool,
    KChar,
    KDecl,
    KDef,
    KElse,
    KElseif,
    KEnd,
    KExit,
    KFalse,
    KFor,
    KHead,
    KIf,
    KInt,
    KList,
    KMod,
    KNew,
    KNil,
    KNilQ,
    KNot,
    KOr,
    KRef,
    KReturn,
    KSkip,
    KTail,
    KTrue,
    // Math Symbols
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Hash,
    // Logical Symbols
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Great,
    GreatOrEqual,
    // Seperator Symbols
    LParenthesis,
    RParenthesis,
    LBracket,
    RBracket,
    Comma,
    Semicolon,
    Colon,
    Assignement, // :=
    Empty,
    NewLine, // Will be helpful with error messages
    Space,   // Whitespace, tabs, etc..
    NotStarted,
    // Somehinγ that doen't get translated to ast node but I want to know what happend
    // Type(start, finish)
    Comment(usize, usize),
    ErrorCRWithoutCF(usize),
    ErrorIntOverflow(usize, usize),
    ErrorCharDefenition(usize, usize),
    ErrorCharSpecial(char), // \n \t \r " ' directly on the inpute stream,
    ErrorStringInComplete(usize, usize),
    ErrorNotParsed(char),
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub token: Token<'a>,
    stream: &'a str,
    index: usize,
}

impl<'a> Parser<'a> {
    pub fn new(stream: &'a str) -> Self {
        Parser {
            token: Token::NotStarted,
            stream,
            index: 0,
        }
    }

    // try to read a single character from the sream
    // Ok: the character read and how much I advanced the index,
    // Err: the error token to return
    pub fn read_char(&mut self) -> Result<char, Token<'a>> {
        let mut chr = self.stream[self.index..].chars().peekmore();
        let start = self.index;

        // check that there is smething after the first tick
        self.index += 1;
        match chr.next() {
            Some('\\') => {
                self.index += 1;
                let ch = match chr.next() {
                    Some('n') => Some('\n'),
                    Some('t') => Some('\t'),
                    Some('r') => Some('\r'),
                    Some('0') => Some('\0'),
                    Some('\\') => Some('\\'),
                    Some('\'') => Some('\''),
                    Some('\"') => Some('\"'),
                    Some('x') => {
                        self.index += 2;
                        let c0 = chr.next();
                        let c1 = chr.next();
                        if c0.is_some()
                            && c1.is_some()
                            && c0.unwrap().is_ascii_hexdigit()
                            && c1.unwrap().is_ascii_hexdigit()
                        {
                            let n0 = c0.unwrap().to_digit(16).unwrap();
                            let n1 = c1.unwrap().to_digit(16).unwrap();
                            let code = (n0 * 10 + n1) as u8;
                            // println!("{} {} = {}", n0, n1, code);
                            Some(code as char)
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
                // println!("{:?}", ch);
                if ch.is_none() {
                    Err(Token::ErrorCharDefenition(start, self.index))
                } else {
                    Ok(ch.unwrap())
                }
            } // Escaped character should look to the next character,
            // Special control character on the input stream, should be checked on there one
            Some('\n') => Err(Token::ErrorCharSpecial('\n')),
            Some('\r') => Err(Token::ErrorCharSpecial('\r')),
            // Some('\t') => Err(Token::ErrorCharSpecial('\t')),
            Some('\"') => Err(Token::ErrorCharSpecial('\"')),
            Some('\'') => Err(Token::ErrorCharSpecial('\'')),
            Some(e) => Ok(e),
            // Some(e) => Token::Char(e), // General, not escaped character
            _ => Err(Token::ErrorCharDefenition(start, self.index)),
        }
    }

    pub fn next_token(&mut self) {
        let mut chr = self.stream[self.index..].chars().peekmore();
        // println!("Index: {:?}", self.index);
        // println!("Remaining: {:?}", self.stream[self.index..].to_string());
        if let Some(x) = chr.next() {
            match x {
                '0'..='9' => {
                    let start = self.index;
                    let mut ch = chr.next();
                    while ch.is_some() && ch.unwrap().is_digit(10) {
                        ch = chr.next();
                        self.index += 1;
                    }
                    // println!("{}",self.stream[start..=self.index].to_string());
                    if let Ok(result) = self.stream[start..=self.index].parse::<i16>() {
                        self.token = Token::INT(result);
                    } else {
                        // println!("{:?}", self.stream[start..self.index].parse::<i16>());
                        self.token = Token::ErrorIntOverflow(start, self.index);
                    }
                }
                'a'..='z' | 'A'..='Z' => {
                    let start = self.index;
                    let mut ch = chr.next();
                    'l: while ch.is_some() {
                        let c = ch.unwrap();
                        // println!("{}", c);
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '?' => self.index += 1,
                            _ => break 'l,
                        }
                        ch = chr.next();
                    }
                    // filter keywords, everything else is a name
                    self.token = match &self.stream[start..self.index + 1] {
                        "and" => Token::KAnd,
                        "bool" => Token::KBool,
                        "char" => Token::KChar,
                        "decl" => Token::KDecl,
                        "def" => Token::KDef,
                        "else" => Token::KElse,
                        "elsif" => Token::KElseif,
                        "end" => Token::KEnd,
                        "exit" => Token::KExit,
                        "false" => Token::KFalse,
                        "for" => Token::KFor,
                        "head" => Token::KHead,
                        "if" => Token::KIf,
                        "int" => Token::KInt,
                        "list" => Token::KList,
                        "mod" => Token::KMod,
                        "new" => Token::KNew,
                        "nil" => Token::KNil,
                        "nil?" => Token::KNilQ,
                        "not" => Token::KNot,
                        "or" => Token::KOr,
                        "ref" => Token::KRef,
                        "return" => Token::KReturn,
                        "skip" => Token::KSkip,
                        "tail" => Token::KTail,
                        "true" => Token::KTrue,

                        e => Token::Name(e),
                    };
                    // Everything else is a Name
                }
                '\'' => {
                    let start = self.index;
                    // check that there is smething after the first tick
                    self.index += 1;
                    self.token = match self.read_char() {
                        Ok(c) => {
                            let c0 = self.read_char();
                            match c0 {
                                Ok(_) => Token::ErrorCharDefenition(start,self.index),
                                Err(Token::ErrorCharSpecial('\'')) => Token::CChar(c),
                                Err(e) => e,
                            }
                        }
                        Err(e) => e,
                    }
                }
                '\"' => {
                    // check that there is smething after the first tick
                    self.index += 1;
                    let start = self.index;
                    let mut c = self.read_char();
                    while c.is_ok()
                        || (c.is_err() && c.as_ref().unwrap_err() == &Token::ErrorCharSpecial('\''))
                    {
                        c = self.read_char();
                        // println!("{:?}", c);
                    }
                    self.index -= 1;
                    // println!("{:?}",self.stream.chars().nth(self.index));
                    self.token = match c.unwrap_err() {
                        Token::ErrorCharSpecial('\"') => Token::CString(&self.stream[start..self.index]),
                        _ => Token::ErrorStringInComplete(start,self.index),
                         
                    }
                }
                '%' => {
                    // println!("Parsing Singline Comment");
                    // Single line commnent
                    let mut ch = chr.next();
                    let start = self.index;
                    while ch.is_some() && ch.unwrap() != '\n' {
                        self.index += 1;
                        ch = chr.next();
                    }
                    self.token = Token::Comment(start, self.index);
                }

                '+' => self.token = Token::Addition,
                '-' => self.token = Token::Subtraction,
                '*' => self.token = Token::Multiplication,
                '/' => self.token = Token::Division,
                '=' => self.token = Token::Equal,
                '#' => self.token = Token::Hash,
                '<' => {
                    // Possible: < <> <=
                    // Peek ahead to check which is the case
                    self.token = match chr.peek() {
                        Some(&'>') => {
                            self.index += 1;
                            Token::NotEqual
                        }
                        Some(&'=') => {
                            self.index += 1;
                            Token::LessOrEqual
                        }
                        Some(&'*') => {
                            // Multiline Comment block
                            let start = self.index;

                            // consume the *
                            self.index += 1;
                            chr.next();

                            let token: Token;
                            loop {
                                let ch = chr.next();
                                match ch {
                                    Some('*') => {
                                        self.index += 1;
                                        if chr.peek() == Some(&'>') {
                                            self.index += 1;
                                            token = Token::Comment(start, self.index);
                                            break;
                                        } else {
                                            continue;
                                        }
                                    }
                                    None => {
                                        token = Token::Comment(start, self.index);
                                        break;
                                    }
                                    _ => self.index += 1,
                                }
                            }
                            token
                        }
                        _ => Token::Less,
                    };
                }
                '>' => {
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            Token::GreatOrEqual
                        }
                        _ => Token::Great,
                    }
                }

                '(' => self.token = Token::LParenthesis,
                ')' => self.token = Token::RParenthesis,
                '[' => self.token = Token::LBracket,
                ']' => self.token = Token::RBracket,
                ',' => self.token = Token::Comma,
                ';' => self.token = Token::Semicolon,
                ':' => {
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            Token::Assignement
                        }
                        _ => Token::Colon,
                    }
                }
                '\r' => {
                    self.token = match chr.next() {
                        Some('\n') => Token::NewLine,
                        _ => Token::ErrorCRWithoutCF(self.index),
                    };
                    self.index += 1;
                }
                '\n' => self.token = Token::NewLine,
                ' ' | '\t' => {
                    let mut ch = chr.next();
                    while ch.is_some()
                        && (ch.unwrap() == ' '
                            || ch.unwrap() == '\n'
                            || ch.unwrap() == '\t'
                            || ch.unwrap() == '\r')
                    {
                        self.index += 1;
                        ch = chr.next();
                    }
                    self.token = Token::Space;
                }
                e => self.token = Token::ErrorNotParsed(e),
            }
        } else {
            self.token = Token::Empty;
        }
        self.index += 1;
    }
}
