use inkwell::{
    module::Module,
    targets::{FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetMachineOptions, TargetTriple}, OptimizationLevel
};
use std::{error::Error, path::PathBuf};

pub fn init_target(triple: &Option<String>) -> Result<Target, Box<dyn Error>> {
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

pub fn machine_from_target(target: &Target, triple: &Option<String>) -> Result<TargetMachine, Box<dyn Error>> {
    let options = TargetMachineOptions::default();
    let triple = if let Some(t) = triple {
        TargetTriple::create(t)
    } else {
        TargetMachine::get_default_triple()
    };

    match target.create_target_machine_from_options(&triple, options) {
        Some(machine) => Ok(machine),
        None => Err("LLVM failed to initialize target machine".into()),
    }
}

pub fn write_code_to_file(
    machine: &TargetMachine,
    module: &Module,
    path: &PathBuf,
    file_type: FileType,
) -> Result<(), Box<dyn Error>> {
    Ok(machine.write_to_file(module, file_type, path)?)
}
