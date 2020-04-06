use core::fmt::Debug;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Scope<T> {
    name: String,
    locals: HashMap<String, SymbolEntry<T>>,
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
                "Variable \"{}\" already defined in function: \"{}\" with Type: {:?}",
                name.as_ref().to_owned(),
                self.name,
                data
            ));
        } else {
            self.locals
                .insert(name.as_ref().to_owned(), SymbolEntry::new(data));
        }
        Ok(())
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<SymbolEntry<T>>> {
        if self.locals.contains_key(name.as_ref()) {
            Some(Rc::new(self.locals[name.as_ref()].clone()))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolEntry<T> {
    pub data: T,
}

impl<T> SymbolEntry<T> {
    pub fn new(data: T) -> Self {
        SymbolEntry { data }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable<T> {
    scopes: Vec<Scope<T>>,
}

impl<T: Debug + Clone> SymbolTable<T> {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new("root")], // the root scope should only have the main function
        }
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<SymbolEntry<T>>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|x| x.lookup(name.as_ref()))
    }

    pub fn insert<S: AsRef<str>>(&mut self, name: S, data: T) -> Result<(), String> {
        match self.scopes.last_mut() {
            Some(k) => k.insert(name, data),
            None => Err("No current scope defined. this should never happen".to_owned()),
        }
    }

    pub fn open_scope<S: AsRef<str>>(&mut self, name: S) {
        // The name of the scope will be the function name
        self.scopes.push(Scope::new(name))
    }
    pub fn close_scope(&mut self) {
        self.scopes.pop();
    }
}
