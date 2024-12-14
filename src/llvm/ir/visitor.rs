use super::*;

use inkwell::values::{AnyValue, BasicValue};
use inkwell::IntPredicate;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{ BasicMetadataTypeEnum, BasicType },
    values::{
        AnyValueEnum,
        BasicValueEnum,
        FunctionValue,
        IntValue,
        PointerValue,
    },
    AddressSpace,
};

pub struct LlvmGenVisitor<'a, 'ctx> {
    alloca_store: AllocaStore<'ctx>,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    curr_fn: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub fn generate(
        ast: &Expr<'_>,
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) {
        let visitor = LlvmGenVisitor::new(context, module, builder);
    }

    fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
    ) -> LlvmGenVisitor<'a, 'ctx> {
        LlvmGenVisitor {
            alloca_store: AllocaStore::new(),
            context,
            module,
            builder,
            curr_fn: None
        }
    }

    fn run(&mut self, ast: &Vec<Decl<'_>>) -> Result<(), Vec<()>> {
        for decl in ast {
            match decl {
                Decl::Var(v) => self.visit_decl_var(v),
                Decl::Function(f) => self.visit_decl_fn(f),
            }
        }
        Ok(())
    }

    fn visit_decl_var(&mut self, var: &decl::Var<'_>) {
        let Some(symbol) = &var.symbol else {
            unreachable!("Symbols shouldn't be None during codegen");
        };

        let symbol = symbol.borrow();

        match symbol.kind() {
            SymbolKind::Global => {
                let r#type = generate_basic_type(&self.context, symbol.r#type());

                self.module.add_global(
                    r#type.as_basic_type_enum(),
                    Some(*ADDRESS_SPACE_GLOBAL),
                    var.name,
                );
            }
            SymbolKind::Local { num } => {
                self.alloca_store.store_local(
                    &self.builder,
                    self.curr_fn.unwrap(),
                    generate_basic_type(&self.context, symbol.r#type())
                        .as_basic_type_enum(),
                    num,
                );
            }
            SymbolKind::Param {..} => unreachable!("Decl can't be SymbolKind::Param"),
            SymbolKind::Func {..} => unreachable!("decl::Var can't be SymbolKind::Func"),
        }
    }

    fn visit_decl_fn(&mut self, r#fn: &decl::Function<'_>) {
        let llvm_fn = match self.module.get_function(r#fn.name) {
            None => self.gen_fn_prototype(r#fn),
            Some(llvm_fn) => llvm_fn,
        };

        if let Some(body) = &r#fn.body {
            let block = self.context.append_basic_block(
                llvm_fn,
                &format!("{}_enter", r#fn.name)
            );
            self.builder.position_at_end(block);

            self.curr_fn = Some(llvm_fn);
            self.alloca_store.store_fn(llvm_fn, block);

            self.visit_stmt(body);

            self.curr_fn = None;
        }
    }

    fn gen_fn_prototype(&self, r#fn: &decl::Function<'_>) -> FunctionValue<'ctx> {
        let Type::Function(fn_type) = &*r#fn.r#type else { unreachable!() };

        let llvm_fn_type = {
                let param_types = fn_type.params.iter()
                    .map(|param| {
                        generate_basic_type(&self.context, &param.r#type)
                            .into()
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                if let Type::Atomic(Atomic::Void, ..) = &*fn_type.return_type {
                    self.context.void_type().fn_type(&param_types, false)
                } else {
                    generate_basic_type(&self.context, &*fn_type.return_type)
                        .fn_type(&param_types, false)
                }
        };

        let llvm_fn = self.module.add_function(
            &r#fn.name,
            llvm_fn_type,
            None
        );

        llvm_fn.get_param_iter()
            .zip(&fn_type.params)
            .for_each(|(llvm_param, param)| llvm_param.set_name(&param.ident));

        llvm_fn
    }

    fn visit_expr(&mut self, expr: &Expr<'_>) -> BasicValueEnum<'ctx> {
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
        expr: &expr::BinaryExpr<'_>
    ) -> BasicValueEnum<'ctx> {
        let (BasicValueEnum::IntValue(left), BasicValueEnum::IntValue(right))
            = (self.visit_expr(&*expr.left), self.visit_expr(&*expr.right))
            else {
                unreachable!("Typechecking should catch non-integer/boolean binary expressions")
            };
        
        match expr.kind {
            expr::BinaryExprKind::Assign => todo!(),
            expr::BinaryExprKind::Add => {
                self.builder
                    .build_int_add(left, right, "addtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Sub => {
                self.builder
                    .build_int_sub(left, right, "subtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Mul => {
                self.builder
                    .build_int_mul(left, right, "multmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Div => {
                self.builder
                    .build_int_signed_div(left, right, "divtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Exp => todo!(),
            expr::BinaryExprKind::Mod => {
                self.builder
                    .build_int_signed_rem(left, right, "modtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Eq => {
                self.builder
                    .build_int_compare(IntPredicate::EQ, left, right, "eqtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::NotEq => {
                self.builder
                    .build_int_compare(IntPredicate::NE, left, right, "netmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::And => {
                self.builder
                    .build_and(left, right, "andtmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Or => {
                self.builder
                    .build_or(left, right, "ortmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Less => {
                self.builder
                    .build_int_compare(IntPredicate::SLT, left, right, "lttmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::LessEq => {
                self.builder
                    .build_int_compare(IntPredicate::SLE, left, right, "letmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::Greater => {
                self.builder
                    .build_int_compare(IntPredicate::SGT, left, right, "gttmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
            expr::BinaryExprKind::GreaterEq => {
                self.builder
                    .build_int_compare(IntPredicate::SGE, left, right, "getmp")
                    .unwrap()
                    .as_basic_value_enum()
            }
        }
    }

    fn visit_expr_ident(
        &mut self,
        ident_expr: &expr::IdentExpr<'_>
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

    fn visit_stmt(&mut self, stmt: &Stmt<'_>) {
        match stmt {
            Stmt::Block(stmts, ..) => for stmt in stmts { self.visit_stmt(stmt) },
            Stmt::Decl(decl, ..) => {
                let Decl::Var(var) = &**decl else { unreachable!() };
                self.visit_decl_var(var);
            }
            Stmt::Expr(expr, ..) => { self.visit_expr(expr); }
            Stmt::Print(exprs, ..) => for expr in exprs {
                todo!()
            },
            Stmt::Return(expr, ..) => todo!(),
            Stmt::If(if_stmt) => { self.visit_stmt_if(if_stmt); }
            Stmt::For(for_stmt) => { self.visit_stmt_for(for_stmt); }
            Stmt::While(while_stmt) => { self.visit_stmt_while(while_stmt); }
        }
    }

    fn visit_stmt_print_char(&self, val: IntValue<'ctx>) {
        let printf = self.get_printf();

        let format = self.builder.build_global_string_ptr("%c", "fmtstr").unwrap();
        self.builder
            .build_call(
                printf,
                &[
                    format.as_pointer_value().into(),
                    val.into(),
                ],
                "calltmp"
            )
            .unwrap();
    }

    fn visit_stmt_print_int(&self, val: IntValue<'ctx>) {
        let printf = self.get_printf();

        let format = self.builder.build_global_string_ptr("%d", "fmtstr").unwrap();
        self.builder
            .build_call(
                printf,
                &[
                    format.as_pointer_value().into(),
                    val.into(),
                ],
                "calltmp"
            )
            .unwrap();
    }

    fn visit_stmt_print_str(&self, val: PointerValue<'ctx>) {
        let printf = self.get_printf();

        let format = self.builder.build_global_string_ptr("%s", "fmtstr").unwrap();
        self.builder
            .build_call(
                printf,
                &[
                    format.as_pointer_value().into(),
                    val.into(),
                ],
                "calltmp"
            )
            .unwrap();
    }

    /// Retrieves the definition of `printf()` from the module, or defines it if
    /// it doesn't already exist.
    ///
    /// The code of the function isn't provided - the compiler will rely on
    /// an installation of libc to provide the implementation. `printf` is
    /// defined as a function taking in a variable amount of pointer values and
    /// returning a 32-bit integer.
    fn get_printf(&self) -> FunctionValue<'ctx> {
        match self.module.get_function("printf") {
            Some(f) => f,
            None => self.module.add_function(
                "printf",
                self.context
                    .i32_type()
                    .fn_type(
                        &[
                            BasicMetadataTypeEnum::PointerType(
                                self.context.ptr_type(AddressSpace::default())
                            ),
                        ],
                        true
                    ),
                None
            )
        }
    }

    fn visit_stmt_if(&mut self, if_stmt: &stmt::IfStmt<'_>) {
        let condition = match self.visit_expr(&*if_stmt.condition) {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };

        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.i64_type().const_zero(),
                "ifcond",
            )
            .unwrap();
        
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");
        
        let then = self.context.append_basic_block(curr_fn, "then");
        let r#else = match &if_stmt.else_body {
            Some(_) => Some(self.context.append_basic_block(curr_fn, "else")),
            None => None,
        };
        let cont = self.context.append_basic_block(curr_fn, "cont");

        self.builder.build_conditional_branch(
            cmp,
            then,
            if let Some(r#else) = r#else { r#else } else { cont },
        );

        self.builder.position_at_end(then);
        self.visit_stmt(&*if_stmt.body);
        self.builder.build_unconditional_branch(cont);

        if let Some(else_body) = &if_stmt.else_body {
            let r#else = r#else.expect("Else block should exist when else body exists");
            self.builder.position_at_end(r#else);
            self.visit_stmt(&**else_body);
            self.builder.build_unconditional_branch(cont);
        }

        self.builder.position_at_end(cont);
    }

    fn visit_stmt_for(&mut self, for_stmt: &stmt::ForStmt<'_>) {
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");
       
        self.visit_expr(&*for_stmt.init_expr);
        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header);

        self.builder.position_at_end(loop_header);
        self.visit_expr(&*for_stmt.init_expr);
        let condition = match self.visit_expr(&*for_stmt.condition) {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };
        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.i64_type().const_zero(),
                "lpcond",
            )
            .unwrap();
        self.builder.build_conditional_branch(cmp, loop_body, cont);

        self.builder.position_at_end(loop_body);
        self.visit_stmt(&*for_stmt.body);
        self.visit_expr(&*for_stmt.next_expr);
        self.builder.build_unconditional_branch(loop_header);

        self.builder.position_at_end(cont);
    }

    fn visit_stmt_while(&mut self, while_stmt: &stmt::WhileStmt<'_>) {
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");

        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header);
        
        self.builder.position_at_end(loop_header);
        let condition = match self.visit_expr(&*while_stmt.condition) {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };
        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.i64_type().const_zero(),
                "lpcond",
            )
            .unwrap();
        self.builder.build_conditional_branch(cmp, loop_body, cont);

        self.builder.position_at_end(loop_body);
        self.visit_stmt(&*while_stmt.body);
        self.builder.build_unconditional_branch(loop_header);

        self.builder.position_at_end(cont);
    }
}

