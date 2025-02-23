use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_stmt(&mut self, stmt: &Stmt<'_>) -> Result<(), BuilderError> {
        match stmt {
            Stmt::Block(stmts, ..) => {
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                    if matches!(stmt, Stmt::Return(..)) {
                        break;
                    }
                }
            },
            Stmt::Decl(decl, ..) => {
                let Decl::Var(var) = &**decl else { unreachable!() };
                self.visit_decl_var(var)?;
            }
            Stmt::Expr(expr, ..) => { self.visit_expr(expr)?; }
            Stmt::Print(exprs, ..) => for expr in exprs {
                let val = self.visit_expr(expr)?;
                match val {
                    BasicValueEnum::IntValue(val) => match expr.get_type().unwrap() {
                        Type::Atomic(Atomic::Integer, ..) => self.visit_stmt_print_int(val)?,
                        Type::Atomic(Atomic::Char, ..) => self.visit_stmt_print_char(val)?,
                        Type::Atomic(Atomic::Boolean, ..) => self.visit_stmt_print_bool(val)?,
                        _ => unreachable!(),
                    }
                    BasicValueEnum::PointerValue(val) => self.visit_stmt_print_str(val)?,
                    _ => unreachable!("Typechecking should catch invalid print types"),
                }
            },
            Stmt::Return(expr, ..) => {
                let val = match expr {
                    Some(expr) => Some(Box::new(self.visit_expr(expr)?) as Box<dyn BasicValue>),
                    None => None,
                };

                self.builder.build_return(val.as_deref())?;
            }
            Stmt::If(if_stmt) => { self.visit_stmt_if(if_stmt)?; }
            Stmt::For(for_stmt) => { self.visit_stmt_for(for_stmt)?; }
            Stmt::While(while_stmt) => { self.visit_stmt_while(while_stmt)?; }
        }

        Ok(())
    }

    fn visit_stmt_print_bool(&self, val: IntValue<'ctx>) -> Result<(), BuilderError> {
        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                val,
                self.context.bool_type().const_zero(),
                "ifcond",
            )?;
        
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");
        
        let then = self.context.append_basic_block(curr_fn, "then");
        let r#else = self.context.append_basic_block(curr_fn, "else");
        let cont = self.context.append_basic_block(curr_fn, "cont");

        self.builder.build_conditional_branch(
            cmp,
            then,
            r#else,
        )?;

        self.builder.position_at_end(then);
        self.visit_stmt_print_str(
            self.builder
                .build_global_string_ptr("true", "truelit")?
                .as_pointer_value()
        )?;
        self.builder.build_unconditional_branch(cont)?;

        self.builder.position_at_end(r#else);
        self.visit_stmt_print_str(
            self.builder
                .build_global_string_ptr("false", "falselit")?
                .as_pointer_value()
        )?;
        self.builder.build_unconditional_branch(cont)?;

        self.builder.position_at_end(cont);

        Ok(())
    }

    fn visit_stmt_print_char(&self, val: IntValue<'ctx>) -> Result<(), BuilderError> {
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
            )?;

        Ok(())
    }

    fn visit_stmt_print_int(&self, val: IntValue<'ctx>) -> Result<(), BuilderError> {
        let printf = self.get_printf();

        let format = self.builder.build_global_string_ptr("%d", "fmtstr")?;
        self.builder
            .build_call(
                printf,
                &[
                    format.as_pointer_value().into(),
                    val.into(),
                ],
                "calltmp"
            )?;

        Ok(())
    }

    fn visit_stmt_print_str(&self, val: PointerValue<'ctx>) -> Result<(), BuilderError> {
        let printf = self.get_printf();

        let format = self.builder.build_global_string_ptr("%s", "fmtstr")?;
        self.builder
            .build_call(
                printf,
                &[
                    format.as_pointer_value().into(),
                    val.into(),
                ],
                "calltmp"
            )?;

        Ok(())
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

    fn visit_stmt_if(
        &mut self,
        if_stmt: &ast::stmt::IfStmt<'_>
    ) -> Result<(), BuilderError> {
        let condition = match self.visit_expr(&*if_stmt.condition)? {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };

        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.bool_type().const_zero(),
                "ifcond",
            )?;
        
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
        )?;

        self.builder.position_at_end(then);
        self.visit_stmt(&*if_stmt.body)?;
        if !then.get_last_instruction().is_some_and(|i| i.is_terminator()) {
            self.builder.build_unconditional_branch(cont)?;
        }

        if let Some(else_body) = &if_stmt.else_body {
            let r#else = r#else.expect("Else block should exist when else body exists");
            self.builder.position_at_end(r#else);
            self.visit_stmt(&**else_body)?;
            if !r#else.get_last_instruction().is_some_and(|i| i.is_terminator()) {
                self.builder.build_unconditional_branch(cont)?;
            }
        }

        self.builder.position_at_end(cont);

        Ok(())
    }

    fn visit_stmt_for(
        &mut self,
        for_stmt: &ast::stmt::ForStmt<'_>
    ) -> Result<(), BuilderError> {
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");
       
        self.visit_expr(&*for_stmt.init_expr)?;
        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header)?;

        self.builder.position_at_end(loop_header);
        self.visit_expr(&*for_stmt.init_expr)?;
        let condition = match self.visit_expr(&*for_stmt.condition)? {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };
        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.bool_type().const_zero(),
                "lpcond",
            )?;
        self.builder.build_conditional_branch(cmp, loop_body, cont)?;

        self.builder.position_at_end(loop_body);
        self.visit_stmt(&*for_stmt.body)?;
        self.visit_expr(&*for_stmt.next_expr)?;
        self.builder.build_unconditional_branch(loop_header)?;

        self.builder.position_at_end(cont);

        Ok(())
    }

    fn visit_stmt_while(
        &mut self,
        while_stmt: &ast::stmt::WhileStmt<'_>
    ) -> Result<(), BuilderError> {
        let curr_fn = self.curr_fn.expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");

        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header)?;
        
        self.builder.position_at_end(loop_header);
        let condition = match self.visit_expr(&*while_stmt.condition)? {
            BasicValueEnum::IntValue(val) => val,
            _ => panic!("Condition expression should evaluate to bool/int"),
        };
        let cmp = self.builder
            .build_int_compare(
                IntPredicate::NE,
                condition,
                self.context.bool_type().const_zero(),
                "lpcond",
            )?;
        self.builder.build_conditional_branch(cmp, loop_body, cont)?;

        self.builder.position_at_end(loop_body);
        self.visit_stmt(&*while_stmt.body)?;
        self.builder.build_unconditional_branch(loop_header)?;

        self.builder.position_at_end(cont);

        Ok(())
    }
}
