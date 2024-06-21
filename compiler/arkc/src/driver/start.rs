use std::fs::File;
use std::io::{self, Read, Write};
use std::path::PathBuf;

use crate::driver::cmd::{self, Args};
use arkc_frontend as language;
use arkc_frontend::sema::{Sema, SemaArgs};
use bytecode::{FunctionData, FunctionId, PackageId, Program};

pub fn start() -> i32 {
    let args = cmd::parse_arguments();

    if let Err(msg) = args {
        cmd::print_help();
        println!();
        println!("{}", msg);
        return 1;
    }

    let mut args = args.unwrap();

    if args.version {
        println!("dora v0.01b");
        return 0;
    }

    if args.help {
        cmd::print_help();
        return 0;
    }

    if args.arg_file.is_none() && !args.command.is_test_boots() {
        eprintln!("missing input argument.");
        return 1;
    }

    let prog = if let Some(ref file) = args.arg_file {
        if file.ends_with(".ark-package") {
            match decode_input_program(&file) {
                Ok(prog) => prog,
                Err(_) => {
                    return 1;
                }
            }
        } else {
            let location = ProgramLocation::File(file.clone());
            match compile_into_program(&args, location) {
                Ok(result) => result,
                Err(_) => {
                    return 1;
                }
            }
        }
    } else {
        assert!(args.command.is_test_boots());
        let content = "fn main() {}";
        let location = ProgramLocation::String(content.to_string());
        match compile_into_program(&args, location) {
            Ok(result) => result,
            Err(_) => {
                return 1;
            }
        }
    };

    // if --check given, stop after type/semantic check
    if args.check {
        return 0;
    }

    if args.command.is_build() {
        return command_build(&args, prog);
    }

    0
}

enum ProgramLocation {
    File(String),
    String(String),
}

/*fn compile_into_program(args: &Args, file: ProgramLocation) -> Result<Program, ()> {
    let (arg_file, test_file_as_string) = match file {
        ProgramLocation::File(file) => (Some(file), None),
        ProgramLocation::String(content) => (None, Some(content)),
    };

    let sem_args = SemaArgs {
        arg_file,
        packages: args.packages.clone(),
        test_file_as_string,
    };

    let mut sa = Sema::new(sem_args);

    let success = language::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if report_errors(&sa) {
        return Err(());
    }

    if report_errors(&sa) {
        return Err(());
    }

    if let Some(ref filter) = args.emit_ast {
        language::emit_ast(&sa, filter);
    }

    // language::generate_bytecode(&sa);

    // Create a serializable data structure from bytecode and metadata.
    // Here we drop the generated AST.
    // let prog = language::emit_program(sa);

    // if let Some(ref filter) = args.emit_bytecode {
    //     language::emit_bytecode(&prog, filter);
    // }

    Ok(prog)
}*/

fn command_build(args: &Args, prog: Program) -> i32 {
    if args.output.is_none() {
        eprintln!("missing output file");
        return 1;
    }

    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");

    let file = args.output.as_ref().expect("missing output");

    match write_program_into_file(&encoded_program, file) {
        Ok(()) => 0,
        Err(_) => {
            eprintln!("Failed to write into output file.");
            1
        }
    }
}

fn decode_input_program(file: &str) -> Result<Program, ()> {
    let encoded_program = match read_input_file(file) {
        Ok(result) => result,
        Err(_) => {
            eprintln!("couldn't read input file.");
            return Err(());
        }
    };

    let config = bincode::config::standard();
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config).expect("serialization failure");
    assert_eq!(decoded_len, encoded_program.len());

    Ok(decoded_prog)
}

fn read_input_file(file: &str) -> Result<Vec<u8>, io::Error> {
    let path = PathBuf::from(file);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => Ok(buffer),
        Err(error) => Err(error),
    }
}

fn write_program_into_file(prog: &[u8], file: &str) -> Result<(), io::Error> {
    let path = PathBuf::from(file);

    let mut f = File::create(&path)?;
    f.write_all(prog)?;
    Ok(())
}

fn encode_and_decode_for_testing(prog: Program) -> Program {
    let config = bincode::config::standard();
    let encoded_program = bincode::encode_to_vec(prog, config).expect("serialization failed");
    let (decoded_prog, decoded_len): (Program, usize) =
        bincode::decode_from_slice(&encoded_program, config).expect("serialization failure");
    assert_eq!(decoded_len, encoded_program.len());

    decoded_prog
}

fn report_errors(sa: &Sema) -> bool {
    if sa.diag.borrow().has_errors() {
        sa.diag.borrow_mut().dump(&sa);
        let no_errors = sa.diag.borrow().errors().len();

        if no_errors == 1 {
            eprintln!("{} error found.", no_errors);
        } else {
            eprintln!("{} errors found.", no_errors);
        }

        true
    } else {
        false
    }
}

fn is_test_fct(fct: &FunctionData) -> bool {
    // the function needs to be marked with the @Test annotation
    fct.is_test
}
