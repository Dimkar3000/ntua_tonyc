use crate::ast::TypeDecl;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone)]
struct Scope {
    name: String,
    locals: HashMap<String, SymbolEntry>,
}

impl Scope {
    pub fn new<S: AsRef<str>>(name: S) -> Self {
        Scope {
            name: name.as_ref().to_owned(),
            locals: HashMap::new(),
        }
    }
    pub fn insert<S: AsRef<str>>(&mut self, name: S, ctype: TypeDecl) -> Result<(), String> {
        assert_ne!(name.as_ref().to_owned().len(), 0);
        if self.locals.contains_key(name.as_ref()) {
            return Err(format!(
                "Variable \"{}\" already defined in function: \"{}\" with Type: {:?}",
                name.as_ref().to_owned(),
                self.name,
                ctype
            ));
        } else {
            self.locals
                .insert(name.as_ref().to_owned(), SymbolEntry::new(ctype));
        }
        Ok(())
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<SymbolEntry>> {
        let a = if self.locals.contains_key(name.as_ref()) {
            Some(Rc::new(self.locals[name.as_ref()].clone()))
        } else {
            None
        };
        if a.is_some() {
            println!(
                "lookup: name: {:?} of type {:?}, found in scope: {}",
                name.as_ref(),
                a.as_ref(),
                self.name
            );
        }
        a
    }
}

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub ctype: TypeDecl,
}

impl SymbolEntry {
    pub fn new(ctype: TypeDecl) -> Self {
        SymbolEntry { ctype }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::new("root")], // the root scope should only have the main function
        }
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<Rc<SymbolEntry>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|x| x.lookup(name.as_ref()))
    }

    pub fn insert<S: AsRef<str>>(&mut self, name: S, ctype: TypeDecl) -> Result<(), String> {
        match self.scopes.last_mut() {
            Some(k) => k.insert(name, ctype),
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
