use super::*;

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub(super) fn visit_decl_var(&mut self, var: &ast::decl::Var<'_>) {
        let Some(symbol) = &var.symbol else {
            unreachable!("Symbols shouldn't be None during codegen");
        };

        let val = match &var.rhs {
            Some(expr) => self.visit_expr(&expr),
            None => self.generate_basic_type(&var.r#type).const_zero(),
        };

        let symbol = symbol.borrow();

        match symbol.kind() {
            SymbolKind::Global => {
                let r#type = self.generate_basic_type(symbol.r#type());

                self.module
                    .add_global(
                        r#type.as_basic_type_enum(),
                        Some(*ADDRESS_SPACE_GLOBAL),
                        var.name,
                    )
                    .set_initializer(&val);
            }
            SymbolKind::Local { num } => {
                let alloca = self.alloca_store.store_local(
                    &self.builder,
                    self.curr_fn.unwrap(),
                    self.generate_basic_type(symbol.r#type())
                        .as_basic_type_enum(),
                    num,
                );

                self.builder.build_store(alloca, val);
            }
            SymbolKind::Param {..} => unreachable!("Decl can't be SymbolKind::Param"),
            SymbolKind::Func {..} => unreachable!("decl::Var can't be SymbolKind::Func"),
        }
    }

    pub(super) fn visit_decl_fn(&mut self, r#fn: &ast::decl::Function<'_>) {
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

    fn gen_fn_prototype(&self, r#fn: &ast::decl::Function<'_>) -> FunctionValue<'ctx> {
        let Type::Function(fn_type) = &*r#fn.r#type else { unreachable!() };

        let llvm_fn_type = {
            let param_types = fn_type.params.iter()
                .map(|param| {
                    self.generate_basic_type(&param.r#type)
                        .into()
                })
                .collect::<Vec<BasicMetadataTypeEnum>>();

            if let Type::Atomic(Atomic::Void, ..) = &*fn_type.return_type {
                self.context.void_type().fn_type(&param_types, false)
            } else {
                self.generate_basic_type(&*fn_type.return_type)
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
}
