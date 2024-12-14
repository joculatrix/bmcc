use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_expr(&mut self, expr: &Expr<'_>) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Ident(ident_expr) => self.visit_expr_ident(ident_expr),
            Expr::BoolLit(val, ..) => {
                let val = if *val { 1 } else { 0 };
                self.context.bool_type()
                    .const_int(val as u64, false)
                    .as_basic_value_enum()
            }
            Expr::CharLit(val, ..) => {
                assert!(val.is_ascii());
                self.context.i8_type()
                    .const_int(*val as u64, false)
                    .as_basic_value_enum()
            }
            Expr::IntLit(val, ..) => {
                self.context.i64_type()
                    .const_int(*val as u64, true)
                    .as_basic_value_enum()
            }
            Expr::StrLit(..) => todo!(),
            Expr::Index(index_expr) => todo!(),
            Expr::Binary(binary_expr) => self.visit_expr_binary(binary_expr),
            Expr::Inc(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr) else {
                    unreachable!("Typechecking should catch non-integer increment")
                };

                self.builder
                    .build_int_add(
                        val,
                        self.context.i64_type().const_int(1, true),
                        "inctmp"
                    )
                    .unwrap()
                    .as_basic_value_enum()
            }
            Expr::Dec(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr) else {
                    unreachable!("Typechecking should catch non-integer decrement")
                };

                self.builder
                    .build_int_sub(
                        val,
                        self.context.i64_type().const_int(1, true),
                        "dectmp"
                    )
                    .unwrap()
                    .as_basic_value_enum()
            }
            Expr::Neg(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr) else {
                    unreachable!("Typechecking should catch non-integer negation")
                };

                self.builder
                    .build_int_neg(val, "negtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            Expr::Not(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr) else {
                    unreachable!("Typechecking should catch non-bool (int) NOT")
                };
                
                self.builder
                    .build_not(val, "nottmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            Expr::Call(call_expr) => {
                let Some(llvm_fn) = self.module.get_function(call_expr.ident) else {
                    unreachable!("Undefined references should be caught in name resolution")
                };

                assert_eq!(call_expr.args.len(), llvm_fn.get_params().len());
                let args = call_expr.args
                    .iter()
                    .map(|arg| self.visit_expr(arg).into())
                    .collect::<Vec<_>>();
                
                self.builder
                    .build_call(llvm_fn, &args, "calltmp")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap()
            }
            Expr::Array(..) => todo!(),
        }
    }

    fn visit_expr_binary(
        &mut self,
        expr: &ast::expr::BinaryExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right))
            = (self.visit_expr(&*expr.left), self.visit_expr(&*expr.right))
            else {
                unreachable!("Typechecking should catch non-integer/boolean binary expressions")
            };
        
        match expr.kind {
            ast::expr::BinaryExprKind::Assign => todo!(),
            ast::expr::BinaryExprKind::Add => {
                self.builder
                    .build_int_add(left, right, "addtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Sub => {
                self.builder
                    .build_int_sub(left, right, "subtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Mul => {
                self.builder
                    .build_int_mul(left, right, "multmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Div => {
                self.builder
                    .build_int_signed_div(left, right, "divtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Exp => todo!(),
            ast::expr::BinaryExprKind::Mod => {
                self.builder
                    .build_int_signed_rem(left, right, "modtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Eq => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "eqtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::NotEq => {
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "netmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::And => {
                self.builder
                    .build_and(left, right, "andtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Or => {
                self.builder
                    .build_or(left, right, "ortmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Less => {
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lttmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::LessEq => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "letmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::Greater => {
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gttmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            ast::expr::BinaryExprKind::GreaterEq => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "getmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
        }
    }

    fn visit_expr_ident(
        &mut self,
        ident_expr: &ast::expr::IdentExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let Some(ref symbol) = ident_expr.symbol else {
            unreachable!("Symbols shouldn't be None during codegen")
        };
        let symbol = symbol.borrow();
        let ptr = match symbol.kind() {
            SymbolKind::Global => {
                self.module
                    .get_global(symbol.ident())
                    .expect("Undefined references should be caught in name resolution")
                    .as_pointer_value()
            }
            SymbolKind::Local { num } => {
                *self.alloca_store
                    .get_local(self.curr_fn.unwrap(), num)
                    .expect("Undefined references should be caught in name resolution")
            }
            SymbolKind::Param { num } => {
                self.curr_fn
                    .expect("A global expression shouldn't have function parameters to reference")
                    .get_nth_param(num as u32)
                    .unwrap()
                    .into_pointer_value()
            }
            SymbolKind::Func { .. } => {
                unreachable!("Expr::Ident shouldn't be SymbolKind::Func")
            }
        };
        match symbol.r#type() {
            Type::Atomic(atomic, ..) => match atomic {
                Atomic::Boolean => {
                    self.builder
                        .build_load(self.context.bool_type(), ptr, "loadtmp")
                        .unwrap()
                }
                Atomic::Char => {
                    self.builder
                        .build_load(self.context.i8_type(), ptr, "loadtmp")
                        .unwrap()
                }
                Atomic::Integer => {
                    self.builder
                        .build_load(self.context.i64_type(), ptr, "loadtmp")
                        .unwrap()
                }
                Atomic::String => ptr.as_basic_value_enum(),
                Atomic::Void => unreachable!("Value shouldn't be type void"),
            }
            Type::Array(..) => ptr.as_basic_value_enum(),
            Type::Function(..) => unreachable!(),
        }
    }
}
