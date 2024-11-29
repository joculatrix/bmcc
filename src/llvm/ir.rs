use std::collections::HashMap;

use crate::ast::*;
use crate::symbol::*;

use inkwell::AddressSpace;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module, values::GlobalValue,
};

pub struct LlvmGenVisitor<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
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
        builder: &'a Builder<'ctx>
    ) -> LlvmGenVisitor<'a, 'ctx> {
        LlvmGenVisitor { context, module, builder }
    }

    fn run(&self, ast: &Vec<Decl<'_>>) -> Result<(), Vec<()>> {
        for decl in ast {
            match decl {
                Decl::Var(v) => self.visit_decl_var(v),
                Decl::Function(f) => self.visit_decl_fn(f),
            }
        }
        Ok(())
    }

    fn visit_decl_var(&self, var: &decl::Var<'_>) {
        let Some(symbol) = &var.symbol else {
            unreachable!("Symbols shouldn't be None during codegen");
        };

        let mut symbol = symbol.borrow();

        match symbol.kind() {
            SymbolKind::Global => {
                let r#type = match symbol.r#type() {
                    Type::Atomic(atomic, ..) => match atomic {
                        Atomic::Boolean => self.context.bool_type(),
                        Atomic::Char => self.context.i8_type(),
                        Atomic::Integer => self.context.i64_type(),
                        Atomic::String => unreachable!(),
                        Atomic::Void => self.context.void_type(),
                    },
                    Type::Array(..) => (),
                    Type::Function(..) => unreachable!("decl::Var can't have type Function"),
                };

                self.module.add_global(r#type, todo!(), var.name);
            }
            SymbolKind::Local => todo!(),
            SymbolKind::Param {..} => unreachable!("Decl can't be SymbolKind::Param"),
            SymbolKind::Func {..} => unreachable!("decl::Var can't be SymbolKind::Func"),
        }
    }

    fn visit_decl_fn(&self, fun: &decl::Function<'_>) {
        let Some(body) = &fun.body else { return; };

        todo!();
    }
}
