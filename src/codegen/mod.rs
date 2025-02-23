use crate::llvm;
use std::{error::Error, fs::File, path::PathBuf};
use inkwell::{context::Context, module::Module, targets::{FileType, TargetMachine}};

use crate::{
    ast::Decl,
    Emit, Linker,
};

pub struct EmitConfig {
    pub emit: Emit,
    pub output: Option<PathBuf>,
    pub target: Option<String>,
    pub linker: Option<Linker>,
}

pub fn codegen(ast: &Vec<Decl<'_>>, config: EmitConfig) -> Result<(), Box<dyn Error>> {
    let ctxt = Context::create();
    let module = ctxt.create_module("bminor");
    let builder = ctxt.create_builder();

    let target = llvm::target::init_target(&config.target)?;
    let machine = llvm::target::machine_from_target(&target, &config.target)?;

    module.set_data_layout(&machine.get_target_data().get_data_layout());

    let visitor = llvm::LlvmGenVisitor::new(&ctxt, &module, &builder);
    let errs = visitor.resolve(ast);
    if errs.len() != 0 {
        let num_errs = errs.len();
        errs.into_iter().for_each(|err| {
            eprintln!(
                "{} {}",
                <str as yansi::Paint>::red("LLVM:").bold(),
                err,
            );
        });
        eprintln!(
            "{} compilation failed due to {} error(s)",
            <String as yansi::Paint>::red(&String::from("ERROR:")).bold(),
            num_errs,
        );
        std::process::exit(1);
    }

    emit(config, &machine, &module)?;

    Ok(())
}

/// Output the module in the requested format, including linking if outputting
/// an executable.
fn emit(
    config: EmitConfig,
    machine: &TargetMachine,
    module: &Module
) -> Result<(), Box<dyn Error>> {
    match config.emit {
        Emit::Executable => {
            let out_path = get_output_path(config.output, "a")?;
            open_file(&out_path)?; // ensure file and parent directories are created
            let obj_path = get_output_path(Some(out_path.with_file_name("a.o")), "")?;
            open_file(&obj_path)?;

            llvm::target::write_code_to_file(&machine, &module, &obj_path, FileType::Object)?;
            link(config.linker, &obj_path, &out_path)?;
        }
        Emit::Object => {
            let path = get_output_path(config.output, "a.o")?;
            open_file(&path)?;
            llvm::target::write_code_to_file(&machine, &module, &path, FileType::Object)?;
        }
        Emit::Assembly => {
            let path = get_output_path(config.output, "a.s")?;
            open_file(&path)?;
            llvm::target::write_code_to_file(&machine, &module, &path, FileType::Assembly)?;
        }
        Emit::Bitcode => {
            let path = get_output_path(config.output, "a.bc")?;
            open_file(&path)?;
            module.write_bitcode_to_path(&path.as_path());
        },
        Emit::Ast => unreachable!(),
    }
    Ok(())
}

/// Attempt to link the binary.
///
/// If a linker was specified by the user, try the linker and halt if unsuccessful.
/// Otherwise, try known linkers until one works.
fn link(
    linker: Option<Linker>,
    obj: &PathBuf,
    out: &PathBuf
) -> Result<(), Box<dyn Error>> {
    match linker {
        Some(link) => {
            let res = match link {
                Linker::Ld => try_linker(Linker::Ld, obj, out)?,
                Linker::Link => try_linker(Linker::Link, obj, out)?,
                Linker::Lld => try_linker(Linker::Lld, obj, out)?,
            };

            if res {
                Ok(())
            } else {
                Err("couldn't find command for given linker".into())
            }
        },
        None => {
            for link in [Linker::Ld, Linker::Link, Linker::Lld] {
                if try_linker(link, obj, out)? {
                    return Ok(());
                }
            }
            Err("couldn't link binary".into())
        }
    }
}

/// Try invoking a linker to link the binary.
///
/// Returns `Ok(true)` if the linker was invoked successfully, `Ok(false)` if the
/// linker wasn't found, or an `Err` if some other error prevented spawning a
/// child process.
///
/// Destroys the object file if successful.
fn try_linker(
    linker: Linker,
    obj: &PathBuf,
    out: &PathBuf
) -> Result<bool, Box<dyn Error>> {
    let (obj, out) = (obj.to_str().unwrap(), out.to_str().unwrap());
    let (cmd, args) = match linker {
        Linker::Ld => {
            ("ld", [&format!("-o{}", out), "--entry=main", obj])
        }
        Linker::Link => {
            ("Link", [&format!("/OUT:{}", out), "/ENTRY:main", obj])
        }
        Linker::Lld => {
            ("lld", [&format!("-o{}", out), "--entry=main", obj])
        }
    };

    match std::process::Command::new(cmd).args(args).spawn() {
        Ok(mut process) => {
            let exit = process.wait()?;
            // clean up object file
            // std::fs::remove_file(obj)?;
            Ok(exit.success())
        }
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => Ok(false),
            _ => Err(e.into()),
        }
    }
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
