use std::path::PathBuf;

use arkc_frontend::{Sema, SemaArgs};

use clap::{Parser, ValueEnum};

#[derive(Parser)]
#[command(author, about, long_about = None)]
struct CompilerArgs {
    /// Name of the module to build
    #[arg(long, short)]
    files: Vec<PathBuf>,

    /// Name of the module to build
    #[arg(long)]
    module_name: Option<String>,

    /// Specify the package path to operate on (default current directory). This changes
    /// the working directory before any other operation
    #[arg(long)]
    package_path: Option<PathBuf>,

    /// Specify a custom scratch directory path (default .build)
    #[arg(long, default_value = ".build")]
    scratch_path: Option<PathBuf>,

    /// Emits AST to stdout.
    #[arg(long)]
    emit_ast: Option<String>,

    #[arg(long)]
    emit_ir: bool,

    /// Specify the backend to use ('llvm' or 'bytecode'). Note that cranelift is only for debug builds.
    /// Ark will use llvm by default for production builds and bc by default for vm targets,
    /// unless overridden by this flag
    #[arg(long)]
    pub backend: Option<Backend>,

    /// Sets the current optimization level from 0 (no optimization) to 3 (aggressive optimization).
    /// Set to s or z to optimize for size.
    #[arg(short = 'O', default_value = "0", value_parser = validate_opt_argument)]
    pub opt_level: char,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, ValueEnum)]
pub enum Backend {
    Wasm,
    Llvm,
}

fn validate_opt_argument(arg: &str) -> Result<char, &'static str> {
    match arg {
        "0" | "1" | "2" | "3" | "s" | "z" => Ok(arg.chars().next().unwrap()),
        _ => Err("Argument to -O must be one of: 0, 1, 2, 3, s, or z"),
    }
}

fn compile(args: CompilerArgs) {
    // Setup the cache and read from the first file
    // let filename = Path::new(&args.file);
    // let file = File::open(filename);
    // let file = expect!(file, "Could not open file {}\n", filename.display());
    // let mut reader = BufReader::new(file);
    // let mut contents = String::new();

    // let parent = filename.parent().unwrap();

    let sema_args = SemaArgs {
        module_name: args.module_name,
        files: Some(args.files),
    };

    let mut sa = Sema::new(sema_args);
    let success = arkc_frontend::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if !success {
        println!("errors: {:?}", sa.diag.borrow().errors());
        std::process::exit(1);
    }

    if let Some(ref filter) = args.emit_ast {
        arkc_frontend::emit_ast(&sa, filter);
    }

    /*
    let (id, func) = sa
        .functions
        .iter()
        .find(|(id, f)| sa.interner.intern("main") == f.name)
        .expect("main fn to exists");

    let source = sa.file(func.file_id);

    */

    // Phase 6: Codegen
    let default_backend = Backend::Llvm;
    let backend = args.backend.unwrap_or(default_backend);

    match backend {
        Backend::Wasm => {
            if cfg!(feature = "wasm") {
                #[cfg(feature = "wasm")]
                {
                    let hir = sa.compilation.hir.borrow();
                    arkc_codegen_wasm::codegen(&hir[0]).unwrap();
                }
            } else {
                eprintln!("The bc backend is required running in interpreted mode. Recompile ark with --features 'wasm'.");
            }
        }
        Backend::Llvm => {
            if cfg!(feature = "llvm") {
                #[cfg(feature = "llvm")]
                {
                    let hir = sa.compilation.hir.borrow();
                    arkc_codegen_llvm::codegen(&hir[0]);
                }
            } else {
                eprintln!("The llvm backend is required for non-debug builds. Recompile ark with --features 'llvm' to enable optimized builds.");
            }
        }
    }
}

fn main() {
    let args = CompilerArgs::parse();
    compile(args)
}
