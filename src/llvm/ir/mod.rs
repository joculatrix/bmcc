mod alloca;
mod visitor;

use alloca::*;
use inkwell::types::BasicTypeEnum;

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
) -> BasicTypeEnum<'ctx> {
    match r#type {
        Type::Atomic(atomic, ..) => match atomic {
            Atomic::Boolean => context.bool_type().as_basic_type_enum(),
            Atomic::Char => context.i8_type().as_basic_type_enum(),
            Atomic::Integer => context.i64_type().as_basic_type_enum(),
            Atomic::String => context.ptr_type(AddressSpace::default()).as_basic_type_enum(),
            Atomic::Void => unreachable!("variable can't be type Void"),
        },
        Type::Array(a_type) => match a_type.size {
            r#type::ArraySize::Known(size) =>
                generate_basic_type(context, &*a_type.r#type)
                    .array_type(size.try_into().unwrap())
                    .as_basic_type_enum(),
            r#type::ArraySize::Unknown => todo!(),
        },
        Type::Function(..) => unreachable!("variable can't be type Function"),
    }
}
