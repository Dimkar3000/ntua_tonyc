use colored::*;
use core::fmt::Display;
use std::fmt::Debug;

/// Error is a struct that helps generate error messages.
// #[derive(Debug)]
pub struct Error {
    giver: String,
    message: String,
    column: usize,
    line: usize,
    sub_error: Option<Box<Error>>,
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "{} {}{}{}{}{}{}",
            self.giver.bright_red().on_black(),
            "Error:(".bright_red().on_black(),
            self.column.to_string().bright_green().on_black(),
            ",".bright_red().on_black(),
            self.line.to_string().bright_green().on_black(),
            "): ".bright_red().on_black(),
            self.message
        )?;
        if self.sub_error.is_some() {
            write!(f, "\n\t{}", self.sub_error.as_ref().unwrap())
        } else {
            Ok(())
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "{} {}{}{}{}{}{}",
            self.giver.bright_red().on_black(),
            "Error:(".bright_red().on_black(),
            self.column.to_string().bright_green().on_black(),
            ",".bright_red().on_black(),
            self.line.to_string().bright_green().on_black(),
            "): ".bright_red().on_black(),
            self.message
        )?;
        if self.sub_error.is_some() {
            write!(f, "\n\t{}", self.sub_error.as_ref().unwrap())
        } else {
            Ok(())
        }
    }
}

impl<'a> std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.sub_error.as_ref() {
            Some(x) => Some(x),
            None => None,
        }
    }
}

impl<'a, T: AsRef<str>> From<T> for Error {
    fn from(t: T) -> Self {
        Error::with_message(0, 0, t.as_ref(), "Codegen")
    }
}

impl<'a> Error {
    pub fn with_message(column: usize, line: usize, m: &'a str, giver: &'a str) -> Self {
        Error {
            giver: giver.to_owned(),
            sub_error: None,
            message: m.to_owned(),
            column,
            line,
        }
    }

    pub fn extend(self, m: &'a str, giver: &'a str) -> Self {
        let line = self.line;
        let column = self.column;
        Error {
            giver: giver.to_owned(),
            sub_error: Some(Box::new(self)),
            message: m.to_owned(),
            line,
            column,
        }
    }
}
