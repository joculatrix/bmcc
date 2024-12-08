mod alloca;
mod visitor;

use alloca::*;

use std::collections::HashMap;
use std::sync::LazyLock;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::types::BasicType;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use crate::ast::*;
use crate::symbol::*;

/// Constant for readability. ADDRESS_SPACE_GLOBAL is defined as 1 in LLVM's
/// AddressSpace enum.
const ADDRESS_SPACE_GLOBAL: LazyLock<AddressSpace> = LazyLock::new(|| {
    AddressSpace::from(1u16)
});

fn generate_basic_type<'ctx>(
    context: &'ctx Context,
    r#type: &Type<'_>,
) -> Box<dyn BasicType<'ctx> + 'ctx> {
    match r#type {
        Type::Atomic(atomic, ..) => match atomic {
            Atomic::Boolean => Box::new(context.bool_type()),
            Atomic::Char => Box::new(context.i8_type()),
            Atomic::Integer => Box::new(context.i64_type()),
            Atomic::String => todo!(),
            Atomic::Void => unreachable!("variable can't be type Void"),
        },
        Type::Array(a_type) => match a_type.size {
            r#type::ArraySize::Known(size) => Box::new(
                generate_basic_type(context, &*a_type.r#type)
                    .array_type(size.try_into().unwrap())
            ),
            r#type::ArraySize::Unknown => todo!(),
        },
        Type::Function(..) => unreachable!("variable can't be type Function"),
    }
}

