use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    rc::Rc,
};

use crate::ast::*;

pub type SymbolRef<'a> = Rc<RefCell<Symbol<'a>>>;

pub struct SymbolTable<'a> {
    stack: Vec<HashMap<&'a str, SymbolRef<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        SymbolTable { stack: vec![] }
    }

    pub fn scope_is_global(&self) -> bool {
        self.stack.len() == 1
    }

    /// Call when entering a scope during name resolution. Pushes a fresh
    /// `HashMap` for this scope's [`Symbol`]s on to the stack.
    pub fn scope_enter(&mut self) {
        self.stack.push(HashMap::new());
    }

    /// Call when exiting a scope during name resolution. Pops the scope's
    /// `HashMap` from the stack.
    pub fn scope_exit(&mut self) {
        self.stack.pop();
    }

    /// Add a symbol to the current scope. Returns an error if there's no
    /// current scope. Check for already existing symbols before calling this.
    pub fn add_symbol(&mut self, symbol: Symbol<'a>) -> Result<(), Box<dyn Error>> {
        if let Some(table) = self.stack.last_mut() {
            table.insert(symbol.ident, Rc::new(RefCell::new(symbol)));
            Ok(())
        } else {
            Err("no table to add symbol to".into())
        }
    }

    /// Search for a [`Symbol`] with the identifier `ident` in the current
    /// scope and return a counted reference to it if it exists, otherwise
    /// return `None`.
    ///
    /// To search back through all scopes in the stack, use [`get_symbol_r()`].
    ///
    /// [`get_symbol_r()`]: Self::get_symbol_r
    pub fn get_symbol(&self, ident: &'_ str) -> Option<SymbolRef<'a>> {
        if let Some(table) = self.stack.last() {
            if let Some(s) = table.get(&ident) {
                Some(Rc::clone(&s))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Search for a [`Symbol`] with the identifier `ident` in all scopes in
    /// the stack and return a counted reference to it if it exists, otherwise
    /// return `None`.
    ///
    /// To search only the current scope, use [`get_symbol()`].
    ///
    /// [`get_symbol()`]: Self::get_symbol
    pub fn get_symbol_r(&self, ident: &'_ str) -> Option<SymbolRef<'a>> {
        for table in self.stack.iter().rev() {
            if let Some(s) = table.get(&ident) {
                return Some(Rc::clone(&s));
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum SymbolKind {
    Global,
    Local(usize),
    Param,
}

#[derive(Debug)]
pub struct Symbol<'a> {
    ident: &'a str,
    kind: SymbolKind,
    r#type: Type<'a>,
}
