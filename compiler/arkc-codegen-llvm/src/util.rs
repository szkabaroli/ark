use std::{path::PathBuf, process::Command};

pub fn link(object_filename: &str, binary_filename: &str, runtime_lib_path: &str) {
    // call gcc to compile the bitcode to a binary
    let output = format!("-o{}", binary_filename);
    let lib_path = format!("-L{}", runtime_lib_path);

    let mut child = Command::new("gcc")
        .arg(object_filename)
        .arg("-Wno-everything")
        .arg("-O0")
        .arg(lib_path.clone())
        .arg("-lark_rt")
        .arg(output.clone())
        .spawn()
        .unwrap();
    
    println!("gcc {} {} {}", object_filename, lib_path, output);

    // remove the temporary bitcode file
    child.wait().unwrap();
    std::fs::remove_file(object_filename).unwrap();
}

/// Returns the default name of the outputted binary file
/// as a result of compiling the program with the given entry module.
pub fn binary_name(module_name: &str) -> String {
    if cfg!(target_os = "windows") {
        PathBuf::from(module_name)
            .with_extension("exe")
            .to_string_lossy()
            .into()
    } else {
        PathBuf::from(module_name)
            .with_extension("")
            .to_string_lossy()
            .into()
    }
}
