use crate::parser::{Token, TokenExtra, TokenKind};

pub struct Parser<'a> {
    stream: &'a str,
    index: usize,
    token: Token<'a>,
    p_token: Token<'a>,
    p_index: usize,
    pub column: usize,
    pub line: usize,
}

impl<'a> Parser<'a> {
    pub fn new(stream: &'a str) -> Self {
        Parser {
            token: Token {
                kind: TokenKind::NotStarted,
                column: 0,
                line: 0,
                extra: TokenExtra::None,
            },
            p_token: Token {
                kind: TokenKind::NotStarted,
                column: 0,
                line: 0,
                extra: TokenExtra::None,
            },
            stream,
            index: 0,
            p_index: 0,
            column: 0,
            line: 1,
        }
    }
    pub fn read_token(&self) -> Token {
        self.token.clone()
    }
    pub fn previous_token(&self) -> Token {
        self.p_token.clone()
    }

    /// tries to read a single character from the stream
    ///
    /// **Ok**: the character read
    ///
    /// **Err**: the error token to return
    pub fn read_char(&mut self) -> Result<char, Token<'a>> {
        let mut chr = self.stream[self.index..].chars().peekable();
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
                                    let code = (n0 * 16 + n1) as u8;
                                    Some(char::from(code))
                                }
                                _ => None,
                            },
                            _ => None,
                        }
                    }
                    _ => None,
                };
                if let Some(c) = ch {
                    Ok(c)
                } else {
                    Err(Token::make_error(
                        self.column,
                        self.line,
                        &format!(
                            "Failed to Parse escaped sequence: {}",
                            &self.stream[start..=self.index]
                        ),
                    ))
                }
            } // Escaped character should look to the next character,
            Some(e) => Ok(e),
            // Some(e) => Token::Char(e), // General, not escaped character
            None => Err(Token::make_error(
                self.column,
                self.line,
                &"Failed to Parse Character. Stream empty!",
            )),
        }
    }

    /// changes the index and the current character to the previous one
    ///
    /// ***WARNING***: It cannot move back more than once
    pub fn back(&mut self) {
        self.index = self.p_index;
        self.token = self.p_token.clone()
    }

    /// call this function to get the next token that is also useable
    ///
    /// Tokens: **NewLine**, **Space** and **Comment** are skipped
    ///
    /// it also advances the index to the beggining of the end of the token
    pub fn advance_token(&mut self) -> Token {
        self.p_token = self.token.clone();
        self.p_index = self.index;
        self.next_token();
        loop {
            let c = &self.token.kind;
            match c {
                TokenKind::NewLine => {
                    self.column = 0;
                }
                TokenKind::Space => {
                    self.column += 1;
                }
                TokenKind::Comment => (),
                _ => break,
            }
            self.next_token();
        }

        self.token.clone()
    }

    fn next_token(&mut self) {
        if self.index == self.stream.len() {
            self.token = Token {
                kind: TokenKind::Empty,
                column: self.column,
                line: self.line,
                extra: TokenExtra::None,
            };
            return;
        }
        let mut chr = self.stream[self.index..].chars().peekable();
        if let Some(x) = chr.next() {
            match x {
                '0'..='9' => {
                    let start = self.index;
                    let o_column = self.column;
                    let mut ch = chr.next();
                    while ch.is_some() && ch.unwrap().is_digit(10) {
                        ch = chr.next();
                        self.index += 1;
                        self.column += 1;
                    }
                    self.token =
                        Token::make_int(o_column, self.line, &self.stream[start..=self.index]);
                }
                'a'..='z' | 'A'..='Z' => {
                    let start = self.index;
                    let o_column = self.column;
                    let mut ch = chr.next();
                    'l: while ch.is_some() {
                        let c = ch.unwrap();
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '?' => {
                                self.index += 1;
                                self.column += 1;
                            }
                            _ => break 'l,
                        }
                        ch = chr.next();
                    }
                    self.token =
                        Token::from_word(o_column, self.line, &self.stream[start..=self.index]);
                    // Everything else is a Name
                }
                '\'' => {
                    self.index += 1;
                    let o_column = self.column;
                    self.column += 1;
                    self.token = match self.read_char() {
                        Ok(c) => {
                            let c0 = self.read_char();
                            self.index -= 1; // Note(dimkar): this reset the index back because there is a global +1 at the end of next token and it will skip the next token
                            match c0 {
                                Ok('\'') => Token::make_cchar(o_column, self.line, c),
                                Ok(e) => Token::make_error(
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
                    // check that there is something after the first tick
                    let o_column = self.column;
                    self.index += 1;
                    self.column += 1;
                    let mut result = String::new();
                    let mut c = self.read_char();
                    while c.is_ok() {
                        // println!("parser: {}", result);
                        if &self.stream[self.index..self.index + 1] == "\"" {
                            result.push(c.unwrap());
                            c = self.read_char();
                            break;
                        }
                        result.push(c.unwrap());
                        c = self.read_char();
                    }
                    self.index -= 1; // Note(dimkar): this reset the index back because there is a global +1 at the end of next token and it will skip the next token
                    self.token = match c {
                        Err(e) => Token::make_error(
                            o_column,
                            self.line,
                            &format!("Failed to parser String: Underline error: {}", e),
                        ),
                        Ok('\"') => Token::make_cstring(self.column, self.line, result),
                        Ok(e) => unreachable!("this should never happen {}", e),
                    };
                }
                '%' => {
                    // Single line commnent
                    let mut ch = chr.next();
                    let o_column = self.column;
                    let start = self.index;
                    while ch.is_some() && ch.unwrap() != '\n' {
                        self.index += 1;
                        self.column += 1;
                        ch = chr.next();
                    }
                    self.token = Token {
                        kind: TokenKind::Comment,
                        column: o_column,
                        line: self.line,
                        extra: TokenExtra::Comment(start, self.index),
                    };
                }

                '+' => {
                    self.token = Token {
                        kind: TokenKind::Addition,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '-' => {
                    self.token = Token {
                        kind: TokenKind::Subtraction,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '*' => {
                    self.token = Token {
                        kind: TokenKind::Multiplication,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '/' => {
                    self.token = Token {
                        kind: TokenKind::Division,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '=' => {
                    self.token = Token {
                        kind: TokenKind::Equal,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '#' => {
                    self.token = Token {
                        kind: TokenKind::Hash,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '<' => {
                    // Possible: < <> <=
                    // Peek ahead to check which is the case
                    let o_column = self.column;
                    self.token = match chr.peek() {
                        Some(&'>') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::NotEqual,
                                column: o_column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        Some(&'=') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::LessOrEqual,
                                column: o_column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        Some(&'*') => {
                            // Multiline Comment block
                            let start = self.index;
                            let o_line = self.line;

                            // consume the *
                            self.index += 1;
                            self.column += 1;
                            chr.next();

                            let token: Token;
                            loop {
                                let ch = chr.next();
                                match ch {
                                    Some('\n') => {
                                        self.index += 1;
                                        self.line += 1;
                                        self.column = 0;
                                    }
                                    Some('*') => {
                                        self.index += 1;
                                        self.column += 1;
                                        if chr.peek() == Some(&'>') {
                                            self.index += 1;
                                            self.column += 1;
                                            token = Token {
                                                kind: TokenKind::Comment,
                                                column: o_column,
                                                line: o_line,
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
                                            column: self.column,
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
                            column: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        },
                    };
                }
                '>' => {
                    let o_column = self.column;
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::GreatOrEqual,
                                column: o_column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Great,
                            column: o_column,
                            line: self.line,
                            extra: TokenExtra::None,
                        },
                    }
                }

                '(' => {
                    self.token = Token {
                        kind: TokenKind::LParenthesis,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ')' => {
                    self.token = Token {
                        kind: TokenKind::RParenthesis,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                '[' => {
                    self.token = Token {
                        kind: TokenKind::LBracket,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ']' => {
                    self.token = Token {
                        kind: TokenKind::RBracket,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ',' => {
                    self.token = Token {
                        kind: TokenKind::Comma,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ';' => {
                    self.token = Token {
                        kind: TokenKind::Semicolon,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    }
                }
                ':' => {
                    self.token = match chr.peek() {
                        Some(&'=') => {
                            self.index += 1;
                            self.column += 1;
                            Token {
                                kind: TokenKind::Assignement,
                                column: self.column - 1,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token {
                            kind: TokenKind::Colon,
                            column: self.column,
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
                                column: self.column,
                                line: self.line,
                                extra: TokenExtra::None,
                            }
                        }
                        _ => Token::make_error(
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
                            column: self.column,
                            line: self.line,
                            extra: TokenExtra::None,
                        }
                    }
                }
                ' ' | '\t' => {
                    let mut ch = chr.next();
                    while ch.is_some()
                        && (ch.as_ref().unwrap() == &' '
                            || ch.as_ref().unwrap() == &'\n'
                            || ch.as_ref().unwrap() == &'\t'
                            || ch.as_ref().unwrap() == &'\r')
                    {
                        self.index += 1;
                        if ch.as_ref().unwrap() == &'\n' {
                            self.line += 1;
                            self.column = 0;
                        } else {
                            self.column += 1;
                        }
                        ch = chr.next();
                    }
                    self.token = Token {
                        kind: TokenKind::Space,
                        column: self.column,
                        line: self.line,
                        extra: TokenExtra::None,
                    };
                }
                e => {
                    self.token = Token::make_error(
                        self.column,
                        self.line,
                        &format!("Failed to parse character: \"{}\"", e),
                    )
                }
            }
        } else {
            self.token = Token {
                kind: TokenKind::Empty,
                column: self.column,
                line: self.line,
                extra: TokenExtra::None,
            };
        }
        self.index += 1;
        self.column += 1;
    }
}
