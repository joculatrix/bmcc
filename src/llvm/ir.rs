use std::sync::LazyLock;

use crate::ast::*;
use crate::symbol::*;

use inkwell::types::{BasicMetadataTypeEnum, BasicType};
use inkwell::values::FunctionValue;
use inkwell::AddressSpace;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
};

pub struct LlvmGenVisitor<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
}

/// Constant for readability. ADDRESS_SPACE_GLOBAL is defined as 1 in LLVM's
/// AddressSpace enum.
static ADDRESS_SPACE_GLOBAL: LazyLock<AddressSpace> = LazyLock::new(|| {
    AddressSpace::from(1u16)
});

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
            SymbolKind::Local => todo!(),
            SymbolKind::Param {..} => unreachable!("Decl can't be SymbolKind::Param"),
            SymbolKind::Func {..} => unreachable!("decl::Var can't be SymbolKind::Func"),
        }
    }

    fn visit_decl_fn(&self, r#fn: &decl::Function<'_>) {
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
            self.visit_stmt(body);
        }
    }

    fn gen_fn_prototype(&self, r#fn: &decl::Function<'_>) -> FunctionValue<'ctx> {
        let Type::Function(fn_type) = &*r#fn.r#type else { unreachable!() };

        let llvm_fn_type = {
                let param_types = fn_type.params.iter()
                    .map(|param| {
                        (*generate_basic_type(&self.context, &param.r#type))
                            .as_basic_type_enum()
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

    fn visit_stmt(&self, stmt: &Stmt<'_>) {}
}

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
