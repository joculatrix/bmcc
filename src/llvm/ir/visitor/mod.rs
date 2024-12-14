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

mod decl;
mod expr;
mod stmt;

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
                r#type::ArraySize::Unknown => todo!(),
            },
            Type::Function(..) => unreachable!("variable can't be type Function"),
        }
    }
}

