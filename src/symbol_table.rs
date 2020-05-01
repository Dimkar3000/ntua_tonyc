use core::fmt::Debug;
use std::collections::HashMap;

/// A scope of variables that will live the same time.
/// the name of it helps with error messages.
///
/// For this language it's the name of the function that it is bind to.
#[derive(Debug, Clone)]
struct Scope<T> {
    name: String,
    locals: HashMap<String, T>,
}

impl<T: Debug + Clone> Scope<T> {
    pub fn new<S: AsRef<str>>(name: S) -> Self {
        Scope {
            name: name.as_ref().to_owned(),
            locals: HashMap::new(),
        }
    }
    pub fn insert<S: AsRef<str>>(&mut self, name: S, data: T) -> Result<(), String> {
        assert_ne!(name.as_ref().to_owned().len(), 0);
        if self.locals.contains_key(name.as_ref()) {
            return Err(format!(
                "Name \"{}\" already defined in function: \"{}\" with Type: {:?}",
                name.as_ref().to_owned(),
                self.name,
                data
            ));
        } else {
            self.locals.insert(name.as_ref().to_owned(), data);
        }
        Ok(())
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&T> {
        if self.locals.contains_key(name.as_ref()) {
            self.locals.get(name.as_ref())
        } else {
            None
        }
    }
}

/// A generic symbol Table Implementation.
/// It can probably work on its own
#[derive(Debug, Clone, Default)]
pub struct SymbolTable<T> {
    scopes: Vec<Scope<T>>,
}

impl<T: Debug + Clone> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new("root")], // the root scope should only have the main function
        }
    }

    /// Iterates over the scopes backwords and looks for the name.
    /// Returns a referense to T  
    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&T> {
        self.scopes
            .iter()
            .rev()
            .find_map(|x| x.lookup(name.as_ref()))
    }

    /// Trying to insert a new name to the Symbol table.
    ///
    /// Ok: Nothing returned
    ///
    /// Err: return either the scope error or the String: "No current scope defined. this should never happen"
    ///
    /// For this language it ***will be*** the scope error due to usage  
    pub fn insert<S: AsRef<str>>(&mut self, name: S, data: T) -> Result<(), String> {
        match self.scopes.last_mut() {
            Some(k) => k.insert(name, data),
            None => Err("No current scope defined. this should never happen".to_owned()),
        }
    }

    /// Appends a new scopes object to the list
    pub fn open_scope<S: AsRef<str>>(&mut self, name: S) {
        // The name of the scope will be the function name
        // Could be useful for debugging
        self.scopes.push(Scope::new(name))
    }

    /// Removes the last scope on the list
    pub fn close_scope(&mut self) {
        self.scopes.pop();
    }
}
