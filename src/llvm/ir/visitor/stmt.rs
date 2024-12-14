use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_stmt(&mut self, stmt: &Stmt<'_>) {
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

    fn visit_stmt_if(&mut self, if_stmt: &ast::stmt::IfStmt<'_>) {
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

    fn visit_stmt_for(&mut self, for_stmt: &ast::stmt::ForStmt<'_>) {
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

    fn visit_stmt_while(&mut self, while_stmt: &ast::stmt::WhileStmt<'_>) {
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