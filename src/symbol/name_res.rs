use super::*;

/// Type for traversing the AST and resolving variable/function names to
/// [`Symbol`]s.
///
/// # Example
///
/// ```
/// let mut ast: Vec<Decl<'_>> = vec![];
/// 
/// let name_res = NameResVisitor::new();
/// let errs = name_res.resolve(&mut ast);
/// if errs.len() != 0 {
///     // handle errors
/// }
/// ```
pub struct NameResVisitor<'a> {
    errs: Vec<NameResErr<'a>>,
    symbol_table: SymbolTable<'a>,
    /// Counter for variable definitions in the current function.
    fn_var_count: usize,
}

impl<'a> NameResVisitor<'a> {
    pub fn new() -> NameResVisitor<'a> {
        NameResVisitor {
            errs: vec![],
            symbol_table: SymbolTable::new(),
            fn_var_count: 0,
        }
    }

    /// Runs the name resolution traversal of the AST, returning any errors
    /// generated, and consumes the `NameResVisitor` in the process.
    pub fn resolve(mut self, ast: &mut Vec<Decl<'a>>) -> Vec<NameResErr<'a>> {
        self.symbol_table.scope_enter(); // enter global scope
        for decl in ast {
            self.visit_decl(decl);
        }

        if let Some(f) = self.symbol_table.get_symbol("main") {
            match f.borrow().r#type() {
                Type::Function(r#type::FunctionType { return_type, .. }) => {
                    if !return_type.is_int() {
                        self.errs.push(NameResErr::MainNonInt {
                            span: f.borrow().decl_span,
                        });
                    }
                }
                _ => {
                    self.errs.push(NameResErr::NoMain);
                }
            }
        } else {
            self.errs.push(NameResErr::NoMain);
        }

        self.symbol_table.scope_exit();
        self.errs
    }

    fn visit_decl(&mut self, decl: &mut Decl<'a>) {
        match decl {
            Decl::Var(v) => {
                self.visit_decl_var(v);
            }
            Decl::Function(f) => {
                self.visit_decl_fn(f);
            }
        }
    }

    fn visit_decl_fn(&mut self, f: &mut decl::Function<'a>) {
        if let Some(symbol) = self.symbol_table.get_symbol(f.name) {
            let kind = symbol.borrow().kind.clone();
            match kind {
                SymbolKind::Func { ref defined } => {
                    if *defined {
                        self.errs.push(
                            NameResErr::AlreadyExists {
                                symbol: Rc::clone(&symbol),
                                span: f.span,
                            }
                        );
                        return;
                    } else {
                        if *f.r#type != symbol.borrow().r#type {
                            self.errs.push(
                                NameResErr::PrototypeNotMatch {
                                    proto_symbol: Rc::clone(&symbol),
                                    span: f.span,
                                }
                            );
                            return;
                        } else {
                            let SymbolKind::Func { ref mut defined }
                            = symbol.borrow_mut().kind else {
                                panic!();
                            };
                            *defined = true;
                        }
                    }
                }
                _ => {
                    self.errs.push(
                        NameResErr::AlreadyExists {
                            symbol: Rc::clone(&symbol),
                            span: f.span,
                        }
                    );
                    return;
                }
            }
        }

        let Type::Function(ref mut r#type) = *f.r#type.as_mut() else { panic!() };

        let symbol = Symbol {
            ident: f.name,
            kind: SymbolKind::Func { defined: f.body.is_some() },
            r#type: Type::Function(r#type.clone()),
            decl_span: f.span,
        };

        f.symbol = Some(self.symbol_table.add_symbol(symbol)
            .expect("scope must already exist"));

        self.symbol_table.scope_enter();

        r#type.params
            .iter_mut()
            .enumerate()
            .for_each(|(i, p)| self.visit_param(i, p));

        self.fn_var_count = 0;

        if let Some(ref mut body) = f.body {
            self.visit_stmt(body);
        }

        self.symbol_table.scope_exit();
    }

    fn visit_decl_var(&mut self, v: &mut decl::Var<'a>) {
        if let Some(symbol) = self.symbol_table.get_symbol(v.name) {
            self.errs.push(
                NameResErr::AlreadyExists { symbol, span: v.span }
            );
            return;
        }
        let is_global = self.symbol_table.scope_is_global();
        let symbol = Symbol {
            ident: v.name,
            kind: if is_global {
                SymbolKind::Global
            } else {
                let num = self.fn_var_count;
                self.fn_var_count += 1;
                SymbolKind::Local { num }
            },
            r#type: *v.r#type.clone(),
            decl_span: v.span,
        };
        v.symbol = Some(self.symbol_table.add_symbol(symbol)
            .expect("scope must already exist"));

        if let Some(ref mut rhs) = v.rhs {
            self.visit_expr(rhs);
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr<'a>) {
        match expr {
            Expr::Ident(expr) => {
                match self.symbol_table.get_symbol_r(expr.ident) {
                    Some(s) => { expr.symbol = Some(s); }
                    None => {
                        self.errs.push(NameResErr::NotExists {
                            ident: expr.ident,
                            span: expr.span,
                        });
                    }
                }
            }
            Expr::BoolLit(..) => (),
            Expr::CharLit(..) => (),
            Expr::IntLit(..) => (),
            Expr::StrLit(..) => (),
            Expr::Index(expr::IndexExpr { array, index, .. }) => {
                self.visit_expr(array);
                self.visit_expr(index);
            }
            Expr::Binary(expr::BinaryExpr { left, right, .. }) => {
                self.visit_expr(left);
                self.visit_expr(right);
            }
            Expr::Inc(expr, ..)
            | Expr::Dec(expr, ..)
            | Expr::Neg(expr, ..)
            | Expr::Not(expr, ..) => { self.visit_expr(expr); }
            Expr::Call(expr::CallExpr { ident, args, symbol, span }) => {
                match self.symbol_table.get_symbol_r(ident) {
                    Some(s) => { *symbol = Some(s); }
                    None => {
                        self.errs.push(NameResErr::NotExists { ident, span: *span });
                    }
                }
                for arg in args {
                    self.visit_expr(arg);
                }
            }
            Expr::Array(exprs, ..) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
        }
    }

    fn visit_param(&mut self, i: usize, p: &mut r#type::Param<'a>) {
        if let Some(symbol) = self.symbol_table.get_symbol(p.ident) {
            self.errs.push(
                NameResErr::AlreadyExists { symbol, span: p.span }
            );
            return;
        }
        let symbol = Symbol {
            ident: p.ident,
            kind: SymbolKind::Param { num: i },
            r#type: p.r#type.clone(),
            decl_span: p.span,
        };
        p.symbol = Some(self.symbol_table.add_symbol(symbol)
            .expect("scope must already exist"));
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'a>) {
        match stmt {
            Stmt::Block(body, _) => {
                self.symbol_table.scope_enter();

                for stmt in body {
                    self.visit_stmt(stmt);
                }

                self.symbol_table.scope_exit();
            }
            Stmt::Decl(decl, _) => { self.visit_decl(decl); }
            Stmt::Expr(expr, _) => { self.visit_expr(expr); }
            Stmt::Print(exprs, _) => {
                for expr in exprs {
                    self.visit_expr(expr);
                }
            }
            Stmt::Return(expr, _) => {
                if let Some(expr) = expr {
                    self.visit_expr(expr);
                }
            }
            Stmt::If(stmt::IfStmt { condition, body, else_body, .. }) => {
                self.visit_expr(condition);

                self.symbol_table.scope_enter();
                self.visit_stmt(body);
                self.symbol_table.scope_exit();

                if let Some(else_body) = else_body {
                    self.symbol_table.scope_enter();
                    self.visit_stmt(else_body);
                    self.symbol_table.scope_exit();
                }
            }
            Stmt::For(
                stmt::ForStmt { init_expr, condition, next_expr, body, .. }
            ) => {
                self.symbol_table.scope_enter();

                self.visit_expr(init_expr);
                self.visit_expr(condition);
                self.visit_expr(next_expr);
                self.visit_stmt(body);
                
                self.symbol_table.scope_exit();
            }
            Stmt::While(stmt::WhileStmt { condition, body, .. }) => {
                self.symbol_table.scope_enter();

                self.visit_expr(condition);
                self.visit_stmt(body);
            
                self.symbol_table.scope_exit();
            }
        }
    }
}

pub enum NameResErr<'a> {
    /// A symbol by that name in the local scope has already been declared
    AlreadyExists {
        symbol: SymbolRef<'a>,
        span: SimpleSpan,
    },
    /// A symbol was referenced, but it does not exist
    NotExists {
        ident: &'a str,
        span: SimpleSpan,
    },
    /// The definition of a function does not match its prototype
    PrototypeNotMatch {
        proto_symbol: SymbolRef<'a>,
        span: SimpleSpan,
    },
    /// Function `main` exists, but it does not return an integer
    MainNonInt {
        span: SimpleSpan,
    },
    /// Function `main` does not exist
    NoMain,
}
