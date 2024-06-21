use arkc_frontend_alt::{check_program, hir::SourceFileId, sema::fn_by_name, Sema, SemaArgs};

use clap::{Parser, Subcommand, ValueEnum, ValueHint};

pub fn get_styles() -> clap::builder::Styles {
    clap::builder::Styles::styled()
        .usage(
            anstyle::Style::new()
                .bold()
                .underline()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Yellow))),
        )
        .header(
            anstyle::Style::new()
                .bold()
                .underline()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Yellow))),
        )
        .literal(
            anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Green))),
        )
        .invalid(
            anstyle::Style::new()
                .bold()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Red))),
        )
        .error(
            anstyle::Style::new()
                .bold()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Red))),
        )
        .valid(
            anstyle::Style::new()
                .bold()
                .underline()
                .fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::Green))),
        )
        .placeholder(
            anstyle::Style::new().fg_color(Some(anstyle::Color::Ansi(anstyle::AnsiColor::White))),
        )
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, ValueEnum)]
pub enum Backend {
    Cranelift,
    Llvm,
}

fn validate_opt_argument(arg: &str) -> Result<char, &'static str> {
    match arg {
        "0" | "1" | "2" | "3" | "s" | "z" => Ok(arg.chars().next().unwrap()),
        _ => Err("Argument to -O must be one of: 0, 1, 2, 3, s, or z"),
    }
}

#[derive(Parser, Debug, Clone)]
struct BuildArgs {
    /// Emits AST to stdout.
    #[arg(long)]
    emit_ast: Option<String>,

    #[arg(long)]
    emit_ir: bool,

    /// Specify the backend to use ('llvm' or 'cranelift'). Note that cranelift is only for debug builds.
    /// Ark will use cranelift by default for debug builds and llvm by default for optimized builds,
    /// unless overridden by this flag
    #[arg(long)]
    pub backend: Option<Backend>,

    /// Sets the current optimization level from 0 (no optimization) to 3 (aggressive optimization).
    /// Set to s or z to optimize for size.
    #[arg(short = 'O', default_value = "0", value_parser = validate_opt_argument)]
    pub opt_level: char,
}

#[derive(Subcommand, Debug, Clone)]
#[command(styles=get_styles())]
enum Commands {
    Run,
    Build(BuildArgs),
}

/// Convenience macro for unwrapping a Result or printing an error message and returning () on Err.
macro_rules! expect {( $result:expr , $fmt_string:expr $( , $($msg:tt)* )? ) => ({
    match $result {
        Ok(t) => t,
        Err(_) => {
            print!($fmt_string $( , $($msg)* )? );
            return ();
        },
    }
});}

fn compile(args: BuildArgs) {
    // Setup the cache and read from the first file
    // let filename = Path::new(&args.file);
    // let file = File::open(filename);
    // let file = expect!(file, "Could not open file {}\n", filename.display());
    // let mut reader = BufReader::new(file);
    // let mut contents = String::new();

    // let parent = filename.parent().unwrap();

    let sema_args = SemaArgs {
        packages: vec![(
            "test".to_string(),
            vec![
                "./examples/test.ark".parse().unwrap(),
                "./examples/test2.ark".parse().unwrap(),
            ],
        )],
        arg_file: Some("./main.ark".to_owned()),
    };

    let mut sa = Sema::new(sema_args);
    let success = arkc_frontend_alt::check_program(&mut sa);
    assert_eq!(success, !sa.diag.borrow().has_errors());

    if !success {
        println!("errors: {:?}", sa.diag.borrow().errors())
    }

    let main_fn = fn_by_name(&sa, "main");
    println!("main {:?}", main_fn)

    /*if let Some(ref filter) = args.emit_ast {
        arkc_frontend::emit_ast(&sa, filter);
    }

    let (id, func) = sa
        .functions
        .iter()
        .find(|(id, f)| sa.interner.intern("main") == f.name)
        .expect("main fn to exists");

    let source = sa.file(func.file_id);

    // Phase 6: Codegen
    let default_backend = Backend::Llvm;
    let backend = args.backend.unwrap_or(default_backend);

    match backend {
        Backend::Cranelift => unimplemented!(),
        Backend::Llvm => {
            if cfg!(feature = "llvm") {
                #[cfg(feature = "llvm")]
                arkc_codegen_llvm::run("test".to_string(), source.ast.get().cloned().unwrap());
            } else {
                eprintln!("The llvm backend is required for non-debug builds. Recompile ark with --features 'llvm' to enable optimized builds.");
            }
        }
    }*/
}

fn main() {
    let args = Args::parse();

    match args.cmd {
        Commands::Run => todo!(),
        Commands::Build(args) => compile(args),
    }
}
