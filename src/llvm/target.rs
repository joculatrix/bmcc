use inkwell::{
    module::Module,
    targets::{FileType, InitializationConfig, Target, TargetMachine, TargetTriple}
};
use std::{error::Error, path::PathBuf};

pub(super) fn init_target(triple: Option<&String>) -> Result<Target, Box<dyn Error>> {
    Target::initialize_all(&InitializationConfig::default());

    let triple = if let Some(t) = triple {
        TargetTriple::create(t)
    } else {
        TargetMachine::get_default_triple()
    };

    match Target::from_triple(&triple) {
        Ok(target) => Ok(target),
        Err(e) => Err(Box::new(e)),
    }
}

pub(super) fn write_code_to_file(
    machine: &TargetMachine,
    module: &Module,
    path: &PathBuf,
    file_type: FileType,
) -> Result<(), Box<dyn Error>> {
    Ok(machine.write_to_file(module, file_type, path)?)
}
