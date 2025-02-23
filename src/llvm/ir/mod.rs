mod alloca;
pub mod visitor;

use alloca::*;
use inkwell::types::BasicTypeEnum;

use std::collections::HashMap;
use std::sync::LazyLock;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use inkwell::AddressSpace;

use crate::ast::{self, *};
use crate::symbol::*;

/// Constant for readability. ADDRESS_SPACE_GLOBAL is defined as 1 in LLVM's
/// AddressSpace enum.
const ADDRESS_SPACE_GLOBAL: LazyLock<AddressSpace> = LazyLock::new(|| {
    AddressSpace::from(1u16)
});

