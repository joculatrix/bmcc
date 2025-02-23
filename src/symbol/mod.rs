use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    rc::Rc,
};
use chumsky::span::SimpleSpan;
use crate::ast::*;

mod name_res;
pub use name_res::{NameResVisitor, NameResErr};

/// Type alias for the [`Symbol`] containers used by the [`SymbolTable`] and
/// for the various AST types to reference their associated symbols.
pub type SymbolRef<'a> = Rc<RefCell<Symbol<'a>>>;

/// A type for holding and interfacing with a stack of tables mapping
/// identifiers to [`Symbol`]s in each scope of the program. Used for name
/// resolution -- see [`NameResVisitor`].
#[derive(Debug)]
pub struct SymbolTable<'a> {
    stack: Vec<HashMap<&'a str, SymbolRef<'a>>>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> SymbolTable<'a> {
        SymbolTable { stack: vec![] }
    }

    /// Checks whether the stack currently only contains the global scope.
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

    /// Add a symbol to the current scope. Returns the resulting [`SymbolRef`]
    /// in a `Result`.
    ///
    /// Ensure symbol doesn't already exist before calling.
    ///
    /// # Errors
    ///
    /// Returns an error if there is no current scope. Implementation should
    /// be written to make this impossible, but the error is sitll returned
    /// to force implementation to consider it.
    pub fn add_symbol(&mut self, symbol: Symbol<'a>)
    -> Result<SymbolRef<'a>, Box<dyn Error>> {
        if let Some(table) = self.stack.last_mut() {
            let ident = symbol.ident;
            let symbol = Rc::new(RefCell::new(symbol));
            table.insert(ident, Rc::clone(&symbol));
            Ok(symbol)
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

#[derive(Copy, Clone, Debug)]
pub enum SymbolKind {
    Global,
    Local {
        /// This variable's ordinal position in the function's locals.
        num: usize
    },
    Param {
        /// This parameter's ordinal position in the function's parameters.
        num: usize
    },
    Func {
        /// Whether this function's body has already been defined.
        defined: bool
    },
}

#[derive(Debug)]
/// Type representing the variables or functions referred to by identifiers used
/// in the program. This is useful for typechecking, as well as ensuring
/// identifiers don't reference nonexistent variables or functions, and that
/// the same variable name isn't declared twice in the same scope.
pub struct Symbol<'a> {
    ident: &'a str,
    kind: SymbolKind,
    r#type: Type<'a>,
    /// The location of this symbol's first declaration in the program, for
    /// error reporting - e.g., to be able to say "variable already declared in
    /// this scope, declared here" or "must be assigned to type integer,
    /// declared here".
    decl_span: SimpleSpan,
}

impl<'a> Symbol<'a> {
    pub fn decl_span(&self) -> SimpleSpan { self.decl_span }

    pub fn ident(&self) -> &'a str { self.ident }

    pub fn kind(&self) -> SymbolKind { self.kind }

    pub fn r#type(&self) -> &Type<'a> { &self.r#type }

    pub fn set_size(&mut self, size: r#type::ArraySize) {
        match &mut self.r#type {
            Type::Array(a_type) => { a_type.size = size; }
            _ => (),
        }
    }
}
