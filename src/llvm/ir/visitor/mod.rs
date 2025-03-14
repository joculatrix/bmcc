use crate::AstVisitor;

use super::*;

use inkwell::IntPredicate;
use inkwell::{
    builder::{ Builder, BuilderError },
    context::Context,
    module::Module,
    types::{ BasicMetadataTypeEnum, BasicType },
    values::{
        BasicValue,
        BasicValueEnum,
        FunctionValue,
        IntValue,
        PointerValue,
    },
    AddressSpace,
};

mod decl;
mod expr;
mod stmt;

/// Type for traversing the B-Minor program AST to produce LLVM IR code.
pub struct LlvmGenVisitor<'a, 'ctx> {
    alloca_store: AllocaStore<'ctx>,
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    curr_fn: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> AstVisitor<'_, (), BuilderError> for LlvmGenVisitor<'a, 'ctx> {
    fn visit(self, ast: &Vec<ast::Decl<'_>>) -> Result<(), Vec<BuilderError>> {
        let errs = self.resolve(ast);
        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }
}

impl<'a, 'ctx> LlvmGenVisitor<'a, 'ctx> {
    pub fn new(
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

    fn resolve(mut self, ast: &Vec<Decl<'_>>) -> Vec<BuilderError> {
        let mut errs = vec![];

        for decl in ast {
            let res = match decl {
                Decl::Var(v) => self.visit_decl_var(v),
                Decl::Function(f) => self.visit_decl_fn(f),
            };

            match res {
                Err(e) => errs.push(e),
                Ok(_) => (),
            }
        }

        if let Err(e) = self.module.verify() {
            eprintln!("{}", e);
            std::process::exit(1);
        }

        errs
    }

    /// Generates a [`BasicTypeEnum`] value from a B-Minor AST [`Type`].
    fn generate_basic_type(
        &self,
        r#type: &Type<'_>,
    ) -> BasicTypeEnum<'ctx> {
        match r#type {
            Type::Atomic(atomic, ..) => match atomic {
                Atomic::Boolean => self.context.bool_type().as_basic_type_enum(),
                Atomic::Char => self.context.i8_type().as_basic_type_enum(),
                Atomic::Integer => self.context.i64_type().as_basic_type_enum(),
                Atomic::String => self.context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
                Atomic::Void => unreachable!("variable can't be type Void"),
            },
            Type::Array(a_type) => match a_type.size {
                r#type::ArraySize::Known(size) =>
                    self.generate_basic_type(&*a_type.r#type)
                        .array_type(size.try_into().unwrap())
                        .as_basic_type_enum(),
                r#type::ArraySize::Unknown =>
                    self.context
                        .ptr_type(AddressSpace::default())
                        .as_basic_type_enum(),
            },
            Type::Function(..) => unreachable!("variable can't be type Function"),
        }
    }
}

