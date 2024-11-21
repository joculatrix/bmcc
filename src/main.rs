#![allow(dead_code)]
#![warn(rust_2024_compatibility)]

use std::error::Error;
use std::path::PathBuf;
use chumsky::{input::Input, span::SimpleSpan, Parser};

use parse::{lex, parser};

mod analysis;
mod ast;
mod error;
mod parse;
mod symbol;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source file to compile
    src: PathBuf,
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
