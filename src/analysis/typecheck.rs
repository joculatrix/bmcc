use chumsky::span::SimpleSpan;
use super::*;

/// Type for traversing the AST and checking that the types of expressions
/// are valid.
///
/// # Example
///
/// ```
/// let mut ast: Vec<Decl<'_>> = vec![];
/// 
/// let mut type_checker = TypecheckVisitor::new();
/// if (errs @ type_checker.resolve(&mut ast)).len() != 0 {
///     // handle errors...
/// }
/// ```
pub struct TypecheckVisitor<'a> {
    errs: Vec<TypecheckErr<'a>>,
    curr_return_type: Option<Type<'a>>,
}

impl<'a> TypecheckVisitor<'a> {
    pub fn new() -> TypecheckVisitor<'a> {
        TypecheckVisitor {
            errs: vec![],
            curr_return_type: None,
        }
    }

    /// Runs the typechecking traversal over the AST, returning any errors
    /// generated, and consumes the `TypecheckVisitor` in the process.
    pub fn resolve(mut self, ast: &mut Vec<Decl<'a>>) -> Vec<TypecheckErr<'a>> {
        for decl in ast {
            self.visit_decl(decl);
        }
        self.errs
    }

    fn visit_decl(&mut self, decl: &mut Decl<'a>) {
        match decl {
            Decl::Var(v) => self.visit_decl_var(v),
            Decl::Function(f) => {
                if let Some(body) = &mut f.body {
                    let prev_return = self.curr_return_type.clone();
                    self.curr_return_type = Some(*f.r#type.clone());
                    self.visit_stmt(body);
                    self.curr_return_type = prev_return;
                }
            }
        }
    }

    fn visit_decl_var(&mut self, v: &decl::Var<'a>) {
        if let Some(right) = &v.rhs {
            let Some(right) = self.visit_expr(&right) else {
                return;
            };
            
            if *v.r#type != right {
                self.errs.push(
                    TypecheckErr::AssignMismatch {
                        left: *v.r#type.clone(),
                        right: right.clone()
                    }
                );
            }
            
            let (Type::Array(mut a_type), Type::Array(r_type))
                = (*v.r#type.clone(), right.clone()) else {
                    return;
                };

            match a_type.size {
                r#type::ArraySize::Known(a_size) => {
                    let r#type::ArraySize::Known(r_size) = r_type.size else {
                        return;
                    };

                    if a_size != r_size {
                        self.errs.push(TypecheckErr::WrongSizeArray {
                            expected: a_size,
                            found: r_size,
                            span: v.span,
                        });
                    }
                }
                r#type::ArraySize::Unknown => {
                    if let r#type::ArraySize::Known(r_size) = r_type.size {
                        a_type.size = r#type::ArraySize::Known(r_size);
                        if let Some(symbol) = &v.symbol {
                            symbol.borrow_mut().set_size(a_type.size);
                        }
                    }
                }
            }
        } else { // (if v.rhs == None)
            match &*v.r#type {
                Type::Array(a_type) => {
                    match a_type.size {
                        r#type::ArraySize::Unknown => {
                            self.errs.push(TypecheckErr::NonSizedArray {
                                span: v.span,
                            });
                        }
                        r#type::ArraySize::Known(_) => (),
                    }
                }
                _ => (),
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr<'a>) -> Option<Type<'a>> {
        match expr {
            Expr::Ident(expr) => {
                if let Some(symbol) = &expr.symbol {
                    Some(symbol.borrow().r#type().clone())
                } else {
                    None
                }
            }
            Expr::BoolLit(.., span) =>
                Some(Type::Atomic(Atomic::Boolean, *span)),
            Expr::CharLit(.., span) =>
                Some(Type::Atomic(Atomic::Char, *span)),
            Expr::IntLit(.., span) =>
                Some(Type::Atomic(Atomic::Integer, *span)),
            Expr::StrLit(.., span) =>
                Some(Type::Atomic(Atomic::String, *span)),
            Expr::Index(expr) => self.visit_expr_index(expr),
            Expr::Binary(expr) => self.visit_expr_binary(expr),
            Expr::Inc(e, span) | Expr::Dec(e, span) | Expr::Neg(e, span) => {
                if let Some(r#type) = self.visit_expr(e) {
                    if !r#type.is_int() {
                        self.errs.push(TypecheckErr::ExpectedInt {
                            verb: expr.to_verb()
                                .expect("unary expr should have verb"),
                            found: r#type,
                        });
                        Some(Type::Atomic(Atomic::Integer, *span))
                    } else {
                        Some(r#type)
                    }
                } else {
                    None
                }
            }
            Expr::Not(e, span) => {
                if let Some(r#type) = self.visit_expr(e) {
                    if !r#type.is_bool() {
                        self.errs.push(TypecheckErr::ExpectedBool {
                            verb: expr.to_verb()
                                .expect("unary expr should have verb"),
                            found: r#type,
                        });
                        Some(Type::Atomic(Atomic::Boolean, *span))
                    } else {
                        Some(r#type)
                    }
                } else {
                    None
                }
            }
            Expr::Call(expr) => self.visit_expr_call(expr),
            Expr::Array(exprs, span) => {
                let Some(first_type) = self.visit_expr(&exprs[0]) else {
                    return None;
                };
                for i in 1..exprs.len() {
                    let Some(r#type) = self.visit_expr(&exprs[i]) else {
                        return None;
                    };
                    if r#type != first_type {
                        self.errs.push(
                            TypecheckErr::MixedTypeArray {
                                first_expected: first_type.clone(),
                                found: r#type,
                            }
                        );
                    }
                }
                Some(Type::Array(r#type::ArrayType {
                    r#type: Box::new(first_type),
                    size: r#type::ArraySize::Known(exprs.len()),
                    span: *span, 
                }))
            }
        }
    }

    fn visit_expr_binary(
        &mut self,
        expr: &expr::BinaryExpr<'a>,
    ) -> Option<Type<'a>> {
        let left = self.visit_expr(&expr.left);
        let right = self.visit_expr(&expr.right);
        
        let (Some(left), Some(right)) = (left, right) else {
            return None;
        };

        match expr.kind {
            expr::BinaryExprKind::Assign => {
                if left != right {
                    self.errs.push(
                        TypecheckErr::AssignMismatch {
                            left,
                            right,
                        }
                    );
                }

                return None;
            }
            // expressions that expect and int and return an int
            expr::BinaryExprKind::Add
            | expr::BinaryExprKind::Sub
            | expr::BinaryExprKind::Mul
            | expr::BinaryExprKind::Div
            | expr::BinaryExprKind::Exp
            | expr::BinaryExprKind::Mod => {
                if !matches!(left, Type::Atomic(Atomic::Integer, ..)) {
                    self.errs.push(
                        TypecheckErr::ExpectedInt {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: left,
                        }
                    );
                }

                if !matches!(right, Type::Atomic(Atomic::Integer, ..)) {
                    self.errs.push(
                        TypecheckErr::ExpectedInt {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: right,
                        }
                    );
                }

                Some(Type::Atomic(Atomic::Integer, expr.span))
            }
            // expressions that expect an int and return a bool
            expr::BinaryExprKind::Less
            | expr::BinaryExprKind::LessEq
            | expr::BinaryExprKind::Greater
            | expr::BinaryExprKind::GreaterEq => {
                if !left.is_int() {
                    self.errs.push(
                        TypecheckErr::ExpectedInt {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: left,
                        }
                    );
                }

                if !right.is_int() {
                    self.errs.push(
                        TypecheckErr::ExpectedInt {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: right,
                        }
                    );
                }

                Some(Type::Atomic(Atomic::Boolean, expr.span))
            }
            expr::BinaryExprKind::Eq
            | expr::BinaryExprKind::NotEq => {
                if left != right {
                    self.errs.push(
                        TypecheckErr::CmpDiffTypes { left, right, }
                    );
                }

                Some(Type::Atomic(Atomic::Boolean, expr.span))
            }
            expr::BinaryExprKind::And
            | expr::BinaryExprKind::Or => {
                if !left.is_bool() {
                    self.errs.push(
                        TypecheckErr::ExpectedBool {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: left,
                        }
                    );
                }

                if !right.is_bool() {
                    self.errs.push(
                        TypecheckErr::ExpectedBool {
                            verb: Expr::Binary(expr.clone()).to_verb()
                                .expect("binary expressions should have verbs"),
                            found: right,
                        }
                    );
                }

                Some(Type::Atomic(Atomic::Boolean, expr.span))
            }
        }
    }

    fn visit_expr_call(
        &mut self,
        expr: &expr::CallExpr<'a>,
    ) -> Option<Type<'a>> {
        let Some(ref symbol) = expr.symbol else {
            return None
        };
        // borrow gets dropped too soon if called during the following let
        // as `symbol.borrow().r#type()`
        let symbol = symbol.borrow();

        let Type::Function(f_type) = symbol.r#type() else {
            todo!()
        };

        if expr.args.len() != f_type.params.len() {
            self.errs.push(
                TypecheckErr::WrongNumArgs {
                    expected: f_type.params.len(),
                    found: expr.args.len(),
                    span: expr.span,
                }
            );
        }

        for (arg, param) in expr.args.iter().zip(f_type.params.iter()) {
            if let Some(arg_type) = self.visit_expr(arg) {
                if arg_type != param.r#type {
                    self.errs.push(
                        TypecheckErr::WrongTypeArg {
                            expected: param.r#type.clone(),
                            found: arg_type,
                        }
                    );
                }
            }
        }

        match &*f_type.return_type {
            Type::Atomic(a, _) => Some(Type::Atomic(a.clone(), expr.span)),
            Type::Array(a) => Some(Type::Array(r#type::ArrayType {
                span: expr.span,
                ..a.clone()
            })),
            // returning functions is forbidden and caught by the parser:
            Type::Function(_) => panic!(),
        }
    }

    fn visit_expr_index(
        &mut self,
        expr: &expr::IndexExpr<'a>,
    ) -> Option<Type<'a>> {
        let array = self.visit_expr(&expr.array);
        let index = self.visit_expr(&expr.index);

        // if either type is None, it means an error happened elsewhere;
        // we'll ignore it for typechecking knowing the compiler will
        // halt before codegen
        let (Some(array), Some(index)) = (array, index) else {
            return None;
        };

        let Type::Array(array) = array else {
            self.errs.push(
                TypecheckErr::IndexNonArray { found: array }
            );
            return None;
        };

        if !index.is_int() {
            self.errs.push(
                TypecheckErr::NonIntIndex { found: index }
            );
        }

        match *array.r#type {
            Type::Atomic(a, _) => Some(Type::Atomic(a, expr.span)),
            Type::Array(a) => Some(Type::Array(r#type::ArrayType {
                span: expr.span,
                ..a
            })),
            // arrays of functions are forbidden and caught by the parser:
            Type::Function(_) => panic!(),
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt<'a>) {
        match stmt {
            Stmt::Block(stmts, _) => for stmt in stmts {
                self.visit_stmt(stmt);
            },
            Stmt::Decl(ref mut decl, _) => self.visit_decl(decl),
            Stmt::Expr(ref expr, _) => { self.visit_expr(expr); }
            Stmt::Print(exprs, _) => {
                for expr in exprs {
                    if let Some(
                        found @ Type::Array(..) |
                        found @ Type::Function(..)
                    ) = self.visit_expr(expr) {
                        self.errs.push(TypecheckErr::InvalidPrint { found });
                    }
                }
            }
            Stmt::Return(ref expr, span) => {
                if let Some(expr) = expr {
                    let r#type = self.visit_expr(expr);
                    if let Some(r#type) = r#type {
                        let expected = self.curr_return_type.clone().unwrap();
                        if r#type != expected {
                            self.errs.push(TypecheckErr::WrongTypeReturn {
                                expected,
                                found: r#type,
                            });
                        }
                    }
                } else {
                    match self.curr_return_type.clone().unwrap() {
                        Type::Atomic(Atomic::Void, ..) => (),
                        r#type @ _ => {
                            self.errs.push(TypecheckErr::WrongTypeReturn {
                                expected: r#type,
                                found: Type::Atomic(Atomic::Void, *span),
                            });
                        }
                    }
                }
            }
            Stmt::If(if_stmt) => {
                self.visit_expr(&if_stmt.condition);
                self.visit_stmt(&mut *if_stmt.body);
                if let Some(else_body) = &mut if_stmt.else_body {
                    self.visit_stmt(&mut *else_body);
                }
            }
            Stmt::For(for_stmt) => {
                self.visit_expr(&for_stmt.init_expr);
                self.visit_expr(&for_stmt.condition);
                self.visit_expr(&for_stmt.next_expr);
                self.visit_stmt(&mut *for_stmt.body);
            }
            Stmt::While(while_stmt) => {
                self.visit_expr(&while_stmt.condition);
                self.visit_stmt(&mut *while_stmt.body);
            }
        }
    }
}

pub enum TypecheckErr<'a> {
    AssignMismatch {
        left: Type<'a>,
        right: Type<'a>,
    },
    CmpDiffTypes {
        left: Type<'a>,
        right: Type<'a>,
    },
    ExpectedBool {
        verb: &'static str,
        found: Type<'a>,
    },
    ExpectedInt {
        verb: &'static str,
        found: Type<'a>,
    },
    IndexNonArray {
        found: Type<'a>,
    },
    MixedTypeArray {
        first_expected: Type<'a>,
        found: Type<'a>,
    },
    NonIntIndex {
        found: Type<'a>,
    },
    NonSizedArray {
        span: SimpleSpan,
    },
    ArrayOfFuncs {
        found: Type<'a>,
    },
    InvalidPrint {
        found: Type<'a>,
    },
    WrongNumArgs {
        expected: usize,
        found: usize,
        span: SimpleSpan,
    },
    WrongSizeArray {
        expected: usize,
        found: usize,
        span: SimpleSpan,
    },
    WrongTypeArg {
        expected: Type<'a>,
        found: Type<'a>,
    },
    WrongTypeReturn {
        expected: Type<'a>,
        found: Type<'a>,
    }
}
