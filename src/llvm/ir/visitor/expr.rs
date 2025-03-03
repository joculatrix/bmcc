use inkwell::intrinsics::Intrinsic;

use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_expr(
        &mut self,
        expr: &Expr<'_>
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match expr {
            Expr::Ident(ident_expr) => self.visit_expr_ident(ident_expr, true),
            Expr::BoolLit(val, ..) => {
                let val = if *val { 1 } else { 0 };
                Ok(
                    self.context.bool_type()
                        .const_int(val, false)
                        .as_basic_value_enum()
                )
            }
            Expr::CharLit(val, ..) => {
                assert!(val.is_ascii());
                Ok(
                    self.context.i8_type()
                        .const_int(*val as u64, false)
                        .as_basic_value_enum()
                )
            }
            Expr::IntLit(val, ..) => {
                Ok(
                    self.context.i64_type()
                        .const_int(*val as u64, true)
                        .as_basic_value_enum()
                )
            }
            Expr::StrLit(val, ..) => {
                Ok(
                    self.builder
                        .build_global_string_ptr(val, "str_lit")?
                        .as_basic_value_enum()
                )
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
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr)? else {
                    unreachable!("Typechecking should catch non-integer increment")
                };

                Ok(
                    self.builder
                        .build_int_add(
                            val,
                            self.context.i64_type().const_int(1, true),
                            "inctmp"
                        )?
                        .as_basic_value_enum()
                )
            }
            Expr::Dec(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr)? else {
                    unreachable!("Typechecking should catch non-integer decrement")
                };

                Ok(
                    self.builder
                        .build_int_sub(
                            val,
                            self.context.i64_type().const_int(1, true),
                            "dectmp"
                        )?
                        .as_basic_value_enum()
                )
            }
            Expr::Neg(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr)? else {
                    unreachable!("Typechecking should catch non-integer negation")
                };

                Ok(
                    self.builder
                        .build_int_neg(val, "negtmp")?
                        .as_basic_value_enum()
                )
            }
            Expr::Not(expr, ..) => {
                let BasicValueEnum::IntValue(val) = self.visit_expr(expr)? else {
                    unreachable!("Typechecking should catch non-bool (int) NOT")
                };
                
                Ok(
                    self.builder
                        .build_not(val, "nottmp")?
                        .as_basic_value_enum()
                )
            }
            Expr::Call(call_expr) => {
                let Some(llvm_fn) = self.module.get_function(call_expr.ident) else {
                    unreachable!("Undefined references should be caught in name resolution")
                };

                assert_eq!(call_expr.args.len(), llvm_fn.get_params().len());
                let args = call_expr.args
                    .iter()
                    .map(|arg| Ok(self.visit_expr(arg)?.into()))
                    .collect::<Result<Vec<_>, _>>()?;
                
                Ok(
                    self.builder
                        .build_call(llvm_fn, &args, "calltmp")?
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                )
            }
            Expr::Array(vals, Some(r#type), ..) => {
                let BasicTypeEnum::ArrayType(llvm_type) = self.generate_basic_type(r#type) else {
                    unreachable!("Array literal shouldn't have non-array type")
                };

                match llvm_type.get_element_type() {
                    BasicTypeEnum::ArrayType(array_type) => {
                        let vals = vals.iter()
                            .map(|expr| Ok(self.visit_expr(expr)?.into_array_value()))
                            .collect::<Result<Vec<_>, _>>()?;

                        Ok(array_type.const_array(&vals).as_basic_value_enum())
                    }
                    BasicTypeEnum::IntType(int_type) => {
                        let vals = vals.iter()
                            .map(|expr| Ok(self.visit_expr(expr)?.into_int_value()))
                            .collect::<Result<Vec<_>, _>>()?;

                        Ok(int_type.const_array(&vals).as_basic_value_enum())
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
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let left = match &*expr.left {
            Expr::Ident(expr) => self.visit_expr_ident(&expr, false)?.into_pointer_value(),
            Expr::Index(expr) => self.visit_expr_index(&expr)?.into_pointer_value(),
            _ => unreachable!("Assignment left-hand should be identifier or array index"),
        };

        let right = self.visit_expr(&expr.right)?;
        
        self.builder.build_store(left, right)?;

        // Like C, assignment returns the new value of the assignee
        Ok(right.as_basic_value_enum())
    }

    fn visit_expr_binary(
        &mut self,
        expr: &ast::expr::BinaryExpr<'_>
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right))
            = (self.visit_expr(&expr.left)?, self.visit_expr(&expr.right)?)
            else {
                unreachable!("Typechecking should catch non-integer/boolean binary expressions")
            };
        
        match expr.kind {
            ast::expr::BinaryExprKind::Assign => unreachable!("Assignment should call `visit_expr_assign`"),
            ast::expr::BinaryExprKind::Add => Ok(
                self.builder
                    .build_int_add(left, right, "addtmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Sub => Ok(
                self.builder
                    .build_int_sub(left, right, "subtmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Mul => Ok(
                self.builder
                    .build_int_mul(left, right, "multmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Div => Ok(
                self.builder
                    .build_int_signed_div(left, right, "divtmp")?
                    .as_basic_value_enum()
            ),
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
                    .build_bit_cast(left, self.context.f64_type(), "casttmp")?;
                let right = self.builder
                    .build_bit_cast(right, self.context.f64_type(), "casttmp")?;

                let res = self.builder
                    .build_call(pow, &[left.into(), right.into()], "calltmp")?
                    .try_as_basic_value()
                    .left()
                    .unwrap();

                Ok(
                    self.builder
                        .build_bit_cast(res, self.context.i64_type(), "casttmp")?
                        .as_basic_value_enum()
                )
            }
            ast::expr::BinaryExprKind::Mod => Ok(
                self.builder
                    .build_int_signed_rem(left, right, "modtmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Eq => Ok(
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "eqtmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::NotEq => Ok(
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "netmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::And => Ok(
                self.builder
                    .build_and(left, right, "andtmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Or => Ok(
                self.builder
                    .build_or(left, right, "ortmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Less => Ok(
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lttmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::LessEq => Ok(
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "letmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::Greater => Ok(
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gttmp")?
                    .as_basic_value_enum()
            ),
            ast::expr::BinaryExprKind::GreaterEq => Ok(
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "getmp")?
                    .as_basic_value_enum()
            ),
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
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
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
                *self.alloca_store
                    .get_local(self.curr_fn.unwrap(), num)
                    .expect("Undefined references should be caught in name resolution")
            }
            SymbolKind::Func { .. } => {
                unreachable!("Expr::Ident shouldn't be SymbolKind::Func")
            }
        };

        if get_value {
            match symbol.r#type() {
                Type::Atomic(atomic, ..) => match atomic {
                    Atomic::Boolean => Ok(
                        self.builder
                            .build_load(self.context.bool_type(), ptr, "loadtmp")?
                    ),
                    Atomic::Char => Ok(
                        self.builder
                            .build_load(self.context.i8_type(), ptr, "loadtmp")?
                    ),
                    Atomic::Integer => Ok(
                        self.builder
                            .build_load(self.context.i64_type(), ptr, "loadtmp")?
                    ),
                    Atomic::String => Ok(ptr.as_basic_value_enum()),
                    Atomic::Void => unreachable!("Value shouldn't be type void"),
                }
                Type::Array(..) => Ok(ptr.as_basic_value_enum()),
                Type::Function(..) => unreachable!(),
            }
        } else {
            Ok(ptr.as_basic_value_enum())
        }
    }

    fn visit_expr_index(
        &mut self,
        expr: &ast::expr::IndexExpr<'_>
    ) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        let r#type = self.generate_basic_type(expr.r#type.as_ref().unwrap());
        let array_ptr = self.visit_expr(&expr.array)?.into_pointer_value();

        unsafe { Ok(
            self.builder
                .build_gep(
                    r#type,
                    array_ptr,
                    &[self.visit_expr(&expr.index)?.into_int_value()],
                    "loadtmp"
                )?
                .as_basic_value_enum()
        )}
    }
}
