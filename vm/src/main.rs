#[macro_use]
extern crate jazzlight;

use jazzlight::interp::*;
use thiserror::Error;

use jazzlight::reader::BytecodeReader;
use jazzlight::value::Value;
use std::io::Cursor;

#[derive(Error, Debug)]
pub enum VMError {
    #[error("data store disconnected")]
    IoError(#[from] std::io::Error),
    #[error("unknown data store error")]
    Unknown,
}

fn main() -> Result<(), VMError> {
    let file = std::env::args().nth(1);
    if file.is_none() {
        eprintln!("Please select JazzLight bytecode file");
        std::process::exit(1);
    }
    let file = file.unwrap();

    let contents = std::fs::read(&file)?;

    let mut reader = BytecodeReader {
        bytes: Cursor::new(&contents),
    };
    let module = reader.read_module();
    let vm = get_vm!();
    vm.save_state_exit();
    match vm.interpret(module) {
        Value::Int(x) => std::process::exit(x as _),
        _ => (),
    }

    Ok(())
}
