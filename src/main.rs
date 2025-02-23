#![allow(dead_code)]
#![warn(rust_2024_compatibility)]

use std::{error::Error, time::Instant};
use std::path::PathBuf;
use chumsky::{ input::Input, span::SimpleSpan, Parser };

use parse::{ lex, parser };

mod analysis;
mod ast;
mod codegen;
mod error;
mod llvm;
mod parse;
mod symbol;

/// Simple compiler for the B-Minor toy language from Douglas Thain's
/// "Introduction to Compilers and Language Design".
#[derive(clap::Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    /// Source file to compile
    src: PathBuf,
    /// Output path
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// The type of output to emit
    #[arg(short, long, value_enum, default_value = "executable")]
    emit: Emit,
    /// Target triple of the intended build target; in the form
    /// <arch><sub_arch>-<vendor>-<sys>-<env>, e.g. x86_64-linux-gnu
    #[arg(short, long)]
    target: Option<String>,
    /// Specify a recognized C compiler toolchain for linking. If one isn't specified,
    /// the compiler will try recognized linkers in succession.
    #[arg(short, long)]
    linker: Option<Linker>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
enum Emit {
    /// Emit an executable
    Executable,
    /// Emit object file (.o)
    Object,
    /// Output assembly code (.s)
    Assembly,
    /// Output LLVM bitcode (.bc)
    Bitcode,
    /// Output LLVM IR (.ll)
    LlvmIR,
    /// Output the Abstract Syntax Tree
    Ast,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
enum Linker {
    Clang,
    Gcc,
    Msvc,
}

fn main() -> Result<(), Box<dyn Error>> {
    let start_time = Instant::now();

    let args = {
        use clap::Parser;
        Args::parse()
    };

    let Ok(src) = std::fs::read_to_string(args.src.clone()) else {
        return Err("failed to read file".into())
    };
    
    eprintln!(
        "{:>9} {:#?}",
        <String as yansi::Paint>::blue(&String::from("Compiling")).bold(),
        &args.src,
    );

    let tokens = lex()
        .parse(&src)
        .into_result()
        .unwrap_or_else(|errs| {
            let num_errs = errs.len();
            error::parser_errs(errs, &args.src, &src);
            eprintln!(
                "{:>9} compilation failed due to {} error(s)",
                <String as yansi::Paint>::red(&String::from("Error")).bold(),
                num_errs,
            );
            std::process::exit(1);
        });

    let mut ast = parser()
        .parse(
            <&[(parse::Token, SimpleSpan)] as Input>
                ::spanned(tokens.as_slice(), (src.len()..src.len()).into())
        )
        .into_result()
        .unwrap_or_else(|errs| {
            let num_errs = errs.len();
            eprintln!(
                "{:>9} compilation failed due to {} error(s)",
                <String as yansi::Paint>::red(&String::from("Error")).bold(),
                num_errs,
            );
            error::parser_errs(errs, &args.src, &src);
            std::process::exit(1);
        });

    let name_res = symbol::NameResVisitor::new();
    let errs = name_res.resolve(&mut ast);
    if errs.len() != 0 {
        let num_errs = errs.len();
        error::name_res_errs(errs, &args.src, &src);
        eprintln!(
            "{:>9} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("Error")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    if matches!(args.emit, Emit::Ast) {
        println!("{:#?}", ast);
        return Ok(());
    }

    let type_checker = analysis::TypecheckVisitor::new();
    let errs = type_checker.resolve(&mut ast);
    if errs.len() != 0 {
        let num_errs = errs.len();
        error::typecheck_errs(errs, &args.src, &src);
        eprintln!(
            "{:>9} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("Error")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    let cf_visitor = analysis::ControlFlowVisitor::new();
    let (unreachable, errs) = cf_visitor.resolve(&ast);
    if unreachable.len() != 0 {
        error::warn_unreachable(unreachable, &args.src, &src);
    }
    if errs.len() != 0 {
        let num_errs = errs.len();
        error::control_flow_errs(errs, &args.src, &src);
        eprintln!(
            "{:>9} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("Error")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    codegen::codegen(
        &ast,
        codegen::EmitConfig {
            emit: args.emit,
            output: args.output,
            target: args.target,
            linker: args.linker,
        }
    )?;

    let elapsed = start_time.elapsed();

    eprintln!(
        "{:>9} in {:.2?}",
        <String as yansi::Paint>::blue(&String::from("Finished")).bold(),
        elapsed,
    );

    Ok(())
}

