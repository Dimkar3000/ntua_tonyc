use crate::parser::TokenKind;

/// TokenExtra contains extra info of a token, depending on the case it could be empty
#[derive(Debug, Clone)]
pub enum TokenExtra {
    None,
    /// CString is a String object because the content is not a copy of the original due to the escaped characters
    CString(String),
    Error(String),
    Name(String),
    Comment(usize, usize),
    Cchar(char),
    Int(i16),
}

/// ### Token is the basic block of language
/// it also carries for extra info to help error printing
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub column: usize,
    pub line: usize,
    pub extra: TokenExtra,
}

use std::fmt;
impl fmt::Display for TokenExtra {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            TokenExtra::None => write!(f, "none"),
            TokenExtra::CString(s) => write!(f, "{:?}", s),
            TokenExtra::Error(e) => write!(f, "\"{}\"", e),
            TokenExtra::Name(s) => write!(f, "{}", s),
            TokenExtra::Comment(start, finish) => write!(f, "({}, {})", start, finish),
            TokenExtra::Cchar(c) => write!(f, "'{:?}'", c),
            TokenExtra::Int(n) => write!(f, "{}", n),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "Token({}, {}, {}, {})",
            self.kind, self.line, self.column, self.extra
        )
    }
}

impl Token {
    pub fn get_cstring(&self) -> Result<String, ()> {
        if let TokenExtra::CString(s) = &self.extra {
            Ok(s.to_string())
        } else {
            Err(())
        }
    }
    pub fn get_error(&self) -> Result<String, ()> {
        if let TokenExtra::Error(s) = self.extra.clone() {
            Ok(s)
        } else {
            Err(())
        }
    }
    pub fn get_name(&self) -> Result<String, ()> {
        if let TokenExtra::Name(s) = &self.extra {
            Ok(s.to_owned())
        } else {
            Err(())
        }
    }
    pub fn get_comment(&self) -> Result<(usize, usize), ()> {
        if let TokenExtra::Comment(s, f) = self.extra {
            Ok((s, f))
        } else {
            Err(())
        }
    }
    pub fn get_cchar(&self) -> Result<char, ()> {
        if let TokenExtra::Cchar(c) = self.extra {
            Ok(c)
        } else {
            Err(())
        }
    }
    pub fn get_int(&self) -> Result<i16, ()> {
        if let TokenExtra::Int(n) = self.extra {
            Ok(n)
        } else {
            Err(())
        }
    }

    pub fn make_error<T: AsRef<str>>(column: usize, line: usize, message: T) -> Self {
        Token {
            kind: TokenKind::Error,
            column,
            line,
            extra: TokenExtra::Error(message.as_ref().to_owned()),
        }
    }

    pub fn make_int(column: usize, line: usize, stream: &str) -> Self {
        match stream.parse::<i16>() {
            Ok(e) => Token {
                kind: TokenKind::INT,
                column,
                line,
                extra: TokenExtra::Int(e),
            },
            Err(_) => Token {
                kind: TokenKind::Error,
                column,
                line,
                extra: TokenExtra::Error("Parser: Int overflow".to_owned()),
            },
        }
    }

    pub fn make_cchar(column: usize, line: usize, c: char) -> Self {
        Token {
            kind: TokenKind::CChar,
            column,
            line,
            extra: TokenExtra::Cchar(c),
        }
    }
    pub fn make_cstring(column: usize, line: usize, stream: String) -> Self {
        Token {
            kind: TokenKind::CString,
            column,
            line,
            extra: TokenExtra::CString(stream),
        }
    }
    pub fn from_word(column: usize, line: usize, stream: &str) -> Self {
        match stream {
            "and" => Token {
                kind: TokenKind::KAnd,
                column,
                line,
                extra: TokenExtra::None,
            },
            "bool" => Token {
                kind: TokenKind::KBool,
                column,
                line,
                extra: TokenExtra::None,
            },
            "char" => Token {
                kind: TokenKind::KChar,
                column,
                line,
                extra: TokenExtra::None,
            },
            "decl" => Token {
                kind: TokenKind::KDecl,
                column,
                line,
                extra: TokenExtra::None,
            },
            "def" => Token {
                kind: TokenKind::KDef,
                column,
                line,
                extra: TokenExtra::None,
            },
            "else" => Token {
                kind: TokenKind::KElse,
                column,
                line,
                extra: TokenExtra::None,
            },
            "elsif" => Token {
                kind: TokenKind::KElseif,
                column,
                line,
                extra: TokenExtra::None,
            },
            "end" => Token {
                kind: TokenKind::KEnd,
                column,
                line,
                extra: TokenExtra::None,
            },
            "exit" => Token {
                kind: TokenKind::KExit,
                column,
                line,
                extra: TokenExtra::None,
            },
            "false" => Token {
                kind: TokenKind::KFalse,
                column,
                line,
                extra: TokenExtra::None,
            },
            "for" => Token {
                kind: TokenKind::KFor,
                column,
                line,
                extra: TokenExtra::None,
            },
            "head" => Token {
                kind: TokenKind::KHead,
                column,
                line,
                extra: TokenExtra::None,
            },
            "if" => Token {
                kind: TokenKind::KIf,
                column,
                line,
                extra: TokenExtra::None,
            },
            "int" => Token {
                kind: TokenKind::KInt,
                column,
                line,
                extra: TokenExtra::None,
            },
            "list" => Token {
                kind: TokenKind::KList,
                column,
                line,
                extra: TokenExtra::None,
            },
            "mod" => Token {
                kind: TokenKind::KMod,
                column,
                line,
                extra: TokenExtra::None,
            },
            "new" => Token {
                kind: TokenKind::KNew,
                column,
                line,
                extra: TokenExtra::None,
            },
            "nil" => Token {
                kind: TokenKind::KNil,
                column,
                line,
                extra: TokenExtra::None,
            },
            "nil?" => Token {
                kind: TokenKind::KNilQ,
                column,
                line,
                extra: TokenExtra::None,
            },
            "not" => Token {
                kind: TokenKind::KNot,
                column,
                line,
                extra: TokenExtra::None,
            },
            "or" => Token {
                kind: TokenKind::KOr,
                column,
                line,
                extra: TokenExtra::None,
            },
            "ref" => Token {
                kind: TokenKind::KRef,
                column,
                line,
                extra: TokenExtra::None,
            },
            "return" => Token {
                kind: TokenKind::KReturn,
                column,
                line,
                extra: TokenExtra::None,
            },
            "skip" => Token {
                kind: TokenKind::KSkip,
                column,
                line,
                extra: TokenExtra::None,
            },
            "tail" => Token {
                kind: TokenKind::KTail,
                column,
                line,
                extra: TokenExtra::None,
            },
            "true" => Token {
                kind: TokenKind::KTrue,
                column,
                line,
                extra: TokenExtra::None,
            },

            e => Token {
                kind: TokenKind::Name,
                column,
                line,
                extra: TokenExtra::Name(e.to_owned()),
            },
        }
    }
}
