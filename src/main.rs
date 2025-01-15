#![allow(dead_code)]
#![warn(rust_2024_compatibility)]

use std::{error::Error, fs::File};
use std::path::PathBuf;
use chumsky::{ input::Input, span::SimpleSpan, Parser };

use parse::{ lex, parser };

mod analysis;
mod ast;
mod error;
mod llvm;
mod parse;
mod symbol;

/// Simple compiler for the B-Minor toy language from Douglas Thain's
/// "Introduction to Compilers and Language Design".
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source file to compile
    #[arg(short, long)]
    src: PathBuf,
    /// Output path
    output: Option<PathBuf>,
    /// The type of output to emit
    #[arg(short, long, value_enum, default_value = "executable")]
    emit: Emit,
    /// Target triple of the intended build target; in the form
    /// <arch><sub_arch>-<vendor>-<sys>-<env>, e.g. x86_64-linux-gnu
    #[arg(short, long)]
    target: Option<String>,
    /// Specify a recognized C linker to use. If one isn't specified, the
    /// compiler will try recognized linkers in succession.
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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, clap::ValueEnum)]
enum Linker {
    /// GNU linker
    Ld,
    /// MSVC linker
    Link,
    /// LLVM linker
    Lld,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = {
        use clap::Parser;
        Args::parse()
    };

    let Ok(src) = std::fs::read_to_string(args.src.clone()) else {
        return Err("failed to read file".into())
    };
    
    eprintln!(
        "{} {:#?}",
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
                "{} compilation failed due to {} error(s)",
                <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
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
                "{} compilation failed due to {} error(s)",
                <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
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
            "{} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    let type_checker = analysis::TypecheckVisitor::new();
    let errs = type_checker.resolve(&mut ast);
    if errs.len() != 0 {
        let num_errs = errs.len();
        error::typecheck_errs(errs, &args.src, &src);
        eprintln!(
            "{} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
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
            "{} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    Ok(())
}

fn open_file(path: &PathBuf) -> Result<File, Box<dyn Error>> {
    if path.exists() && !path.is_file() {
        return Err("output path isn't a file name".into());
    }

    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)?;
    }

    Ok(File::create(&path)?)
}

fn get_output_path(
    path: Option<PathBuf>, 
    default: &str
) -> Result<PathBuf, Box<dyn Error>> {
    if let Some(path) = path {
        if path.is_file() || !path.exists() {
            Ok(path)
        } else {
            Err(format!("{:#?} exists and isn't a file", path).into())
        }
    } else {
        Ok(PathBuf::from(default))
    }
}
