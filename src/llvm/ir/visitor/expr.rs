use inkwell::intrinsics::Intrinsic;

use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_expr(&mut self, expr: &Expr<'_>) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Ident(ident_expr) => self.visit_expr_ident(ident_expr, true),
            Expr::BoolLit(val, ..) => {
                let val = if *val { 1 } else { 0 };
                self.context.bool_type()
                    .const_int(val, false)
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
            Expr::StrLit(val, ..) => {
                self.context
                    .const_string(val.as_bytes(), false)
                    .as_basic_value_enum()
            }
            Expr::Index(expr) => self.visit_expr_index(expr),
            Expr::Binary(expr) => {
                if matches!(expr.kind, ast::expr::BinaryExprKind::Assign) {
                    self.visit_expr_assign(expr)
                } else {
                    self.visit_expr_binary(expr)
                }
            }
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
            Expr::Array(vals, Some(r#type), ..) => {
                let BasicTypeEnum::ArrayType(llvm_type) = self.generate_basic_type(r#type) else {
                    unreachable!("Array literal shouldn't have non-array type")
                };

                match llvm_type.get_element_type() {
                    BasicTypeEnum::ArrayType(array_type) => {
                        let vals = vals.iter()
                            .map(|expr| self.visit_expr(expr).into_array_value())
                            .collect::<Vec<_>>();

                        array_type.const_array(&vals).as_basic_value_enum()
                    }
                    BasicTypeEnum::IntType(int_type) => {
                        let vals = vals.iter()
                            .map(|expr| self.visit_expr(expr).into_int_value())
                            .collect::<Vec<_>>();

                        int_type.const_array(&vals).as_basic_value_enum()
                    }
                    _ => unreachable!("Values only use LLVM's Int and Array types"),
                }
            }
            Expr::Array(_, None, ..) => unreachable!("`None` array type shouldn't pass typechecking"),
        }
    }

    fn visit_expr_assign(
        &mut self,
        expr: &ast::expr::BinaryExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let left = match &*expr.left {
            Expr::Ident(expr) => self.visit_expr_ident(&expr, false).into_pointer_value(),
            Expr::Index(expr) => self.visit_expr_index(&expr).into_pointer_value(),
            _ => unreachable!("Assignment left-hand should be identifier or array index"),
        };

        let right = self.visit_expr(&expr.right);
        
        self.builder.build_store(left, right);

        // Like C, assignment returns the new value of the assignee
        right.as_basic_value_enum()
    }

    fn visit_expr_binary(
        &mut self,
        expr: &ast::expr::BinaryExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right))
            = (self.visit_expr(&expr.left), self.visit_expr(&expr.right))
            else {
                unreachable!("Typechecking should catch non-integer/boolean binary expressions")
            };
        
        match expr.kind {
            ast::expr::BinaryExprKind::Assign => unreachable!("Assignment should call `visit_expr_assign`"),
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
            // Exponentiation can get dicey. LLVM doesn't have a basic instruction
            // for it, and manually building a for loop or something here would
            // raise annoying edge cases. For now, exponentiation uses the LLVM
            // pow.f64 intrinsic and some typecasting. Better than reinventing the
            // pow() wheel.
            ast::expr::BinaryExprKind::Exp => {
                let pow = Intrinsic::find("llvm.pow.f64")
                    .unwrap()
                    .get_declaration(
                        self.module,
                        &[
                            self.context.f64_type().into(),
                            self.context.f64_type().into(),
                        ],
                    )
                    .unwrap();

                let left = self.builder
                    .build_bit_cast(left, self.context.f64_type(), "casttmp")
                    .unwrap();
                let right = self.builder
                    .build_bit_cast(right, self.context.f64_type(), "casttmp")
                    .unwrap();

                let res = self.builder
                    .build_call(pow, &[left.into(), right.into()], "calltmp")
                    .unwrap()
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                self.builder.build_bit_cast(res, self.context.i64_type(), "casttmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
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

    /// Retrieves a variable from its identifier.
    ///
    /// If `get_value` is `true`, this function loads the value stored by the
    /// variable (unless it's an array). Otherwise, this function returns a
    /// pointer to the variable (for assignment).
    fn visit_expr_ident(
        &mut self,
        ident_expr: &ast::expr::IdentExpr<'_>,
        get_value: bool,
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

        if get_value {
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
        } else {
            ptr.as_basic_value_enum()
        }
    }

    fn visit_expr_index(
        &mut self,
        expr: &ast::expr::IndexExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let r#type = self.generate_basic_type(expr.r#type.as_ref().unwrap());
        let array_ptr = self.visit_expr(&expr.array).into_pointer_value();

        unsafe {
            self.builder
                .build_gep(
                    r#type,
                    array_ptr,
                    &[self.visit_expr(&expr.index).into_int_value()],
                    "loadtmp"
                )
                .unwrap()
                .as_basic_value_enum()
        }
    }
}
