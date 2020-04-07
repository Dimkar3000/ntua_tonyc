use core::fmt::Debug;
use peekmore::PeekMore;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenKind {
    Addition,
    Assignement,
    CChar,
    CString,
    Colon,
    Comma,
    Comment,
    Division,
    Empty,
    Equal,
    Error,
    Great,
    GreatOrEqual,
    Hash,
    INT,
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
    LBracket,
    LParenthesis,
    Less,
    LessOrEqual,
    Multiplication,
    Name,
    NewLine,
    NotEqual,
    NotStarted,
    RBracket,
    RParenthesis,
    Semicolon,
    Space,
    Subtraction,
}

#[derive(Debug, Clone)]
pub enum TokenExtra {
    None,
    CString(String),
    Error(String),
    Name(String),
    Comment(usize, usize),
    Cchar(char),
    Number(i16),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub col: usize,
    pub line: usize,
    pub extra: TokenExtra,
}

#[derive(Debug)]
pub struct Parser {
    token: Token,
    pub stream: String,
    pub index: usize,
    pub column: usize,
    pub line: usize,
    steps: Vec<usize>,
}

impl Token {
    pub fn get_kind(&self) -> TokenKind {
        self.kind
    }

    fn error(col: usize, line: usize, _message: &str) -> Self {
        Token {
            kind: TokenKind::Error,
            col,
            line,
            extra: TokenExtra::None,
        }
    }
    fn int(col: usize, line: usize, stream: &str) -> Self {
        match stream.parse::<i16>() {
            Ok(e) => Token {
                kind: TokenKind::INT,
                col,
                line,
                extra: TokenExtra::Number(e),
            },
            Err(_) => Token {
                kind: TokenKind::Error,
                col,
                line,
                extra: TokenExtra::Error("Parser: Int overflow".to_owned()),
            },
        }
    }

    fn cchar(col: usize, line: usize, c: char) -> Self {
        Token {
            kind: TokenKind::CChar,
            col,
            line,
            extra: TokenExtra::Cchar(c),
        }
    }
    fn cstring(col: usize, line: usize, stream: String) -> Self {
        Token {
            kind: TokenKind::CString,
            col,
            line,
            extra: TokenExtra::CString(stream),
        }
    }
    fn from_word(col: usize, line: usize, stream: &str) -> Self {
        match stream {
            "and" => Token {
                kind: TokenKind::KAnd,
                col,
                line,
                extra: TokenExtra::None,
            },
            "bool" => Token {
                kind: TokenKind::KBool,
                col,
                line,
                extra: TokenExtra::None,
            },
            "char" => Token {
                kind: TokenKind::KChar,
                col,
                line,
                extra: TokenExtra::None,
            },
            "decl" => Token {
                kind: TokenKind::KDecl,
                col,
                line,
                extra: TokenExtra::None,
            },
            "def" => Token {
                kind: TokenKind::KDef,
                col,
                line,
                extra: TokenExtra::None,
            },
            "else" => Token {
                kind: TokenKind::KElse,
                col,
                line,
                extra: TokenExtra::None,
            },
            "elif" => Token {
                kind: TokenKind::KElseif,
                col,
                line,
                extra: TokenExtra::None,
            },
            "end" => Token {
                kind: TokenKind::KEnd,
                col,
                line,
                extra: TokenExtra::None,
            },
            "exit" => Token {
                kind: TokenKind::KExit,
                col,
                line,
                extra: TokenExtra::None,
            },
            "false" => Token {
                kind: TokenKind::KFalse,
                col,
                line,
                extra: TokenExtra::None,
            },
            "for" => Token {
                kind: TokenKind::KFor,
                col,
                line,
                extra: TokenExtra::None,
            },
            "head" => Token {
                kind: TokenKind::KHead,
                col,
                line,
                extra: TokenExtra::None,
            },
            "if" => Token {
                kind: TokenKind::KIf,
                col,
                line,
                extra: TokenExtra::None,
            },
            "int" => Token {
                kind: TokenKind::KInt,
                col,
                line,
                extra: TokenExtra::None,
            },
            "list" => Token {
                kind: TokenKind::KList,
                col,
                line,
                extra: TokenExtra::None,
            },
            "mod" => Token {
                kind: TokenKind::KMod,
                col,
                line,
                extra: TokenExtra::None,
            },
            "new" => Token {
                kind: TokenKind::KNew,
                col,
                line,
                extra: TokenExtra::None,
            },
            "nil" => Token {
                kind: TokenKind::KNil,
                col,
                line,
                extra: TokenExtra::None,
            },
            "nil?" => Token {
                kind: TokenKind::KNilQ,
                col,
                line,
                extra: TokenExtra::None,
            },
            "not" => Token {
                kind: TokenKind::KNot,
                col,
                line,
                extra: TokenExtra::None,
            },
            "or" => Token {
                kind: TokenKind::KOr,
                col,
                line,
                extra: TokenExtra::None,
            },
            "ref" => Token {
                kind: TokenKind::KRef,
                col,
                line,
                extra: TokenExtra::None,
            },
            "return" => Token {
                kind: TokenKind::KReturn,
                col,
                line,
                extra: TokenExtra::None,
            },
            "skip" => Token {
                kind: TokenKind::KSkip,
                col,
                line,
                extra: TokenExtra::None,
            },
            "tail" => Token {
                kind: TokenKind::KTail,
                col,
                line,
                extra: TokenExtra::None,
            },
            "true" => Token {
                kind: TokenKind::KTrue,
                col,
                line,
                extra: TokenExtra::None,
            },

            e => Token {
                kind: TokenKind::Name,
                col,
                line,
                extra: TokenExtra::Name(e.to_owned()),
            },
        }
    }
}

impl Parser {
    pub fn new<S: Into<String>>(stream: S) -> Self {
        Parser {
            token: Token {
                kind: TokenKind::NotStarted,
                col: 0,
                line: 0,
                extra: TokenExtra::None,
            },
            stream: stream.into(),
            index: 0,
            column: 0,
            line: 1,
            steps: Vec::new(),
        }
    }
    pub fn read_token(&self) -> Token {
        self.token.clone()
    }

    // try to read a single character from the sream
    // Ok: the character read and how much I advanced the index,
    // Err: the error token to return
    pub fn read_char(&mut self) -> Result<char, Token> {
        let mut chr = self.stream[self.index..].chars().peekmore();
        let start = self.index;

        // check that there is smething after the first tick
        self.index += 1;
        self.column += 1;
        match chr.next() {
            Some('\\') => {
                self.index += 1;
                self.column += 1;
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
                        self.column += 2;
                        let c0 = chr.next();
                        let c1 = chr.next();
                        match (c0, c1) {
                            (Some(cc0), Some(cc1)) => match (cc0.to_digit(16), cc1.to_digit(16)) {
                                (Some(n0), Some(n1)) => {
                                    let code = (n0 * 10 + n1) as u8;
                                    Some(char::from(code))
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    }
                    _ => None,
                };
                // println!("{:?}", ch);
                if let Some(c) = ch {
                    Ok(c)
                } else {
                    Err(Token::error(
                        self.column,
                        self.line,
                        &format!(
                            "Failed to Parse escaped sequence: {:?}",
                            &self.stream[start..=self.index]
                        ),
                    ))
                }
            } // Escaped character should look to the next character,
            Some(e) => Ok(e),
            // Some(e) => Token::Char(e), // General, not escaped character
            None => Err(Token::error(
                self.column,
                self.line,
                &"Failed to Parse Character. Stream empty!",
            )),
        }
    }
    pub fn get_token(&mut self) -> Token {
        self.next_token();
        loop {
            let c = &self.token.kind;
            match c {
                TokenKind::NewLine => (),
                TokenKind::Space => (),
                TokenKind::Comment => (),
                _ => break,
            }
            self.next_token();
        }

        self.token.clone()
    }

    // pub fn back(&mut self) {
    //     self.index = self.steps.pop().unwrap_or(0);
    //     self.index = self.steps.pop().unwrap_or(0);
    //     self.next_token();
    // }

    pub fn next_token(&mut self) {
        self.steps.push(self.index);
        if self.index == self.stream.len() {
            self.token = Token {
                kind: TokenKind::Empty,
                col: self.column,
                line: self.line,
                extra: TokenExtra::None,
            };
            return;
        }
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
                        self.column += 1;
                    }
                    // println!("{}",self.stream[start..=self.index].to_string());
                    self.token =
                        Token::int(self.column, self.line, &self.stream[start..=self.index]);
                }
                'a'..='z' | 'A'..='Z' => {
                    let start = self.index;
                    let mut ch = chr.next();
                    'l: while ch.is_some() {
                        let c = ch.unwrap();
                        // println!("{}", c);
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '?' => {
                                self.index += 1;
                                self.column += 1;
                            }
                            _ => break 'l,
                        }
                        ch = chr.next();
                    }
                    // filter keywords, everything else is a name
                    self.token =
                        Token::from_word(self.column, self.line, &self.stream[start..=self.index]);
                    // Everything else is a Name
                }
                '\'' => {
                    // let start = self.index;
                    // check that there is smething after the first tick
                    self.index += 1;
                    self.column += 1;
                    self.token = match self.read_char() {
                        Ok(c) => {
                            let c0 = self.read_char();
                            self.index -=1; // Note(dimkar): this reset the index back because there is a global +1 at the end of next token and it will skip the next token
                            match c0 {
                                Ok('\'') => Token::cchar(self.column, self.line, c),
                                Ok(e) => Token::error(
                                    self.column,
                                    self.line,
                                    &format!(
                                        "Invalid character, Expected \"\'\" but got: \"{}\"",
                                        e
                                    ),
                                ),
                                Err(e) => e,
                            }
                        }
                        Err(e) => e,
                    }
                }
                '\"' => {
                    // check that there is smething after the first tick
                    self.index += 1;
                    self.column += 1;
                    // let start = self.index;
                    let mut result = String::new();
                    let mut c = self.read_char();
                    while c.is_ok() && c.as_ref().unwrap() != &'\"' {
                        result.push(c.unwrap());
                        c = self.read_char();
                    }
                    self.index -=1; // Note(dimkar): this reset the index back because there is a global +1 at the end of next token and it will skip the next token
                    // println!("{:?}",self.stream.chars().nth(self.index));
                    self.token = match c {
                        Err(e) => Token::error(
                            self.column,
                            self.line,
                            &format!("Failed to parser String: Underline error: {:?}", e),
                        ),
                        Ok('\"') => Token::cstring(self.column, self.line, result),
                        Ok(e) => unreachable!("this should never happen {}", e),
                    };
                    // println!("here: {}",self.stream);
                }
                '%' => {
                    // println!("Parsing Singline Comment");
                    // Single line commnent
                    let mut ch = chr.next();
                    let start = self.index;
                    while ch.is_some() && ch.unwrap() != '\n' {
                        self.index += 1;
                        self.column += 1;
                        ch = chr.next();
                    }
                    self.token = Token {
                        kind: TokenKind::Comment,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::Comment(start, self.index),
                    };
                }

                '+' => {
                    self.token = Token {
                        kind: TokenKind::Addition,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '-' => {
                    self.token = Token {
                        kind: TokenKind::Subtraction,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '*' => {
                    self.token = Token {
                        kind: TokenKind::Multiplication,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '/' => {
                    self.token = Token {
                        kind: TokenKind::Division,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '=' => {
                    self.token = Token {
                        kind: TokenKind::Equal,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '#' => {
                    self.token = Token {
                        kind: TokenKind::Hash,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '<' => {
                    // Possible: < <> <=
                    // Peek ahead to check which is the case
                    self.token = match chr.peek() {
                        Some(&'>') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::NotEqual,
                                col: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        Some(&'=') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::LessOrEqual,
                                col: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        Some(&'*') => {
                            // Multiline Comment block
                            let start = self.index;

                            // consume the *
                            self.index += 1;
                            self.column += 1;
                            chr.next();

                            let token: Token;
                            loop {
                                let ch = chr.next();
                                match ch {
                                    Some('*') => {
                                        self.index += 1;
                                        self.column += 1;
                                        if chr.peek() == Some(&'>') {
                                            self.index += 1;
                                            self.column += 1;
                                            token = Token {
                                                kind: TokenKind::Comment,
                                                col: self.column,
                                                line: self.line,
                                                extra: TokenExtra::Comment(start, self.index),
                                            };
                                            break;
                                        } else {
                                            continue;
                                        }
                                    }
                                    None => {
                                        token = Token {
                                            kind: TokenKind::Comment,
                                            col: self.column,
                                            line: self.line,
                                            extra: TokenExtra::Comment(start, self.index),
                                        };
                                        break;
                                    }
                                    _ => {
                                        self.index += 1;
                                        self.column += 1;
                                    }
                                }
                            }
                            token
                        }
                        _ => Token {
                            kind: TokenKind::Less,
                            col: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        },
                    };
                }
                '>' => {
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::GreatOrEqual,
                                col: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Great,
                            col: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        },
                    }
                }

                '(' => {
                    self.token = Token {
                        kind: TokenKind::LParenthesis,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ')' => {
                    self.token = Token {
                        kind: TokenKind::RParenthesis,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '[' => {
                    self.token = Token {
                        kind: TokenKind::LBracket,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ']' => {
                    self.token = Token {
                        kind: TokenKind::RBracket,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ',' => {
                    self.token = Token {
                        kind: TokenKind::Comma,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ';' => {
                    self.token = Token {
                        kind: TokenKind::Semicolon,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ':' => {
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            Token {
                                kind: TokenKind::Assignement,
                                col: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Colon,
                            col: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        },
                    }
                }
                '\r' => {
                    self.token = match chr.next() {
                        Some('\n') => {
                            self.line += 1;
                            self.column = 0;
                            Token {
                                kind: TokenKind::NewLine,
                                col: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token::error(
                            self.column,
                            self.line,
                            "Erro: \\r not followed with \\n isn't allowed, how did you do that :D",
                        ),
                    };
                    self.index += 1;
                }
                '\n' => {
                    self.token = {
                        self.line += 1;
                        self.column = 0;
                        Token {
                            kind: TokenKind::NewLine,
                            col: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        }
                    }
                }
                ' ' | '\t' => {
                    let mut ch = chr.next();
                    while ch.is_some()
                        && (ch.unwrap() == ' '
                            || ch.unwrap() == '\n'
                            || ch.unwrap() == '\t'
                            || ch.unwrap() == '\r')
                    {
                        self.index += 1;
                        self.column += 1;
                        ch = chr.next();
                    }
                    self.token = Token {
                        kind: TokenKind::Space,
                        col: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    };
                }
                e => {
                    self.token = Token::error(
                        self.column,
                        self.line,
                        &format!("Failed to parse character: \"{:?}\"", e),
                    )
                }
            }
        } else {
            self.token = Token {
                kind: TokenKind::Empty,
                col: self.column,
                line: self.line,
                extra: TokenExtra::None,
            };
        }
        self.index += 1;
        self.column += 1;
    }
}
