use std::{path::PathBuf, process::Command};

/// Returns the default name of the outputted binary file
/// as a result of compiling the program with the given entry module.
pub fn binary_name(module_name: &str) -> String {
    PathBuf::from(module_name)
        .with_extension("wasm")
        .to_string_lossy()
        .into()
}
