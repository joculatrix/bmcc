use std::cell::RefCell;
use std::sync::LazyLock;

use crate::ast::*;
use crate::symbol::*;

use inkwell::values::AnyValueEnum;
use inkwell::IntPredicate;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{ BasicMetadataTypeEnum, BasicType },
    values::FunctionValue,
    AddressSpace,
};

pub struct LlvmGenVisitor<'a, 'ctx> {
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    builder: &'a Builder<'ctx>,
    curr_fn: RefCell<Option<FunctionValue<'ctx>>>,
}

/// Constant for readability. ADDRESS_SPACE_GLOBAL is defined as 1 in LLVM's
/// AddressSpace enum.
const ADDRESS_SPACE_GLOBAL: LazyLock<AddressSpace> = LazyLock::new(|| {
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
        builder: &'a Builder<'ctx>,
    ) -> LlvmGenVisitor<'a, 'ctx> {
        LlvmGenVisitor { context, module, builder, curr_fn: RefCell::new(None) }
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

            let mut curr_fn = self.curr_fn.borrow_mut();
            *curr_fn = Some(llvm_fn);

            self.visit_stmt(body);

            *curr_fn = None;
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

    fn visit_expr(&self, expr: &Expr<'_>) -> AnyValueEnum<'ctx> {}

    fn visit_stmt(&self, stmt: &Stmt<'_>) {
        match stmt {
            Stmt::Block(stmts, ..) => for stmt in stmts { self.visit_stmt(stmt) },
            Stmt::Decl(decl, ..) => {
                let Decl::Var(var) = &**decl else { unreachable!() };
                self.visit_decl_var(var);
            }
            Stmt::Expr(expr, ..) => { self.visit_expr(expr); }
            Stmt::Print(vec, ..) => todo!(),
            Stmt::Return(expr, ..) => todo!(),
            Stmt::If(if_stmt) => { self.visit_stmt_if(if_stmt); }
            Stmt::For(for_stmt) => { self.visit_stmt_for(for_stmt); }
            Stmt::While(while_stmt) => { self.visit_stmt_while(while_stmt); }
        }
    }

    fn visit_stmt_if(&self, if_stmt: &stmt::IfStmt<'_>) {
        let condition = match self.visit_expr(&*if_stmt.condition) {
            AnyValueEnum::IntValue(val) => val,
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
        
        let curr_fn = self.curr_fn.borrow().expect("Stmt can't exist outside function");
        
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

    fn visit_stmt_for(&self, for_stmt: &stmt::ForStmt<'_>) {
        let curr_fn = self.curr_fn.borrow().expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");
       
        self.visit_expr(&*for_stmt.init_expr);
        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header);

        self.builder.position_at_end(loop_header);
        self.visit_expr(&*for_stmt.init_expr);
        let condition = match self.visit_expr(&*for_stmt.condition) {
            AnyValueEnum::IntValue(val) => val,
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

    fn visit_stmt_while(&self, while_stmt: &stmt::WhileStmt<'_>) {
        let curr_fn = self.curr_fn.borrow().expect("Stmt can't exist outside function");

        let loop_header = self.context.append_basic_block(curr_fn, "loop");
        let loop_body = self.context.append_basic_block(curr_fn, "body");
        let cont = self.context.append_basic_block(curr_fn, "cont");

        // ensure previous block ends with a branch:
        self.builder.build_unconditional_branch(loop_header);
        
        self.builder.position_at_end(loop_header);
        let condition = match self.visit_expr(&*while_stmt.condition) {
            AnyValueEnum::IntValue(val) => val,
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
