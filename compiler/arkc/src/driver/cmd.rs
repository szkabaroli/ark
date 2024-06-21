use std::default::Default;
use std::path::PathBuf;

// Write the Docopt usage string.
static USAGE: &'static str = "
Usage: arkc test [options] [<file>]
       arkc [options] <file> [--] [<argument>...]
       arkc (--version | --help)

Options:
    -h, --help              Shows this text.
    --version               Shows version.
    --emit-ast=<fct>        Emits AST to stdout.
    --emit-asm=<fct>        Emits assembly code to stdout.
    --emit-asm-file         Emits assembly code into file `dora-<pid>.asm`.
    --emit-asm-boots        Emits assembly code for all boots compilations.
    --emit-graph=<fct>      Emits graph for function.
    --emit-bytecode=<fct>   Emits bytecode to stdout.
    --emit-bytecode-compiler=<fct> Emits bytecode before compilation.
    --emit-stubs            Emits generated stubs.
    --emit-debug=<fct>      Emits debug instruction at beginning of functions.
    --emit-debug-native     Emits debug instruction at beginning of native stub.
    --emit-debug-compile    Emits debug instruction at beginning of compile stub.
    --emit-debug-entry      Emits debug instruction at beginning of entry stub.
    --emit-debug-boots      Emits debug instruction at beginning of boots function.
    --always-boots          Uses boots for all functions in the program.
    --omit-bounds-check     Omit array index out of bounds checks.
    --check                 Only type check given program.
    --asm-syntax TYPE       Emits assembly with Intel or AT&T syntax.
                            Allowed values: intel, att.
    --enable-perf           Enable dump for perf.
    
    --compiler=<name>       Switch default compiler. Possible values: cannon [default: cannon].
    --test-filter=<name>    Filter tests.
    --clear-regs            Clear register when freeing.

    --disable-tlab          Disable tlab allocation.
    --disable-barrier       Disable barriers.

    --code-size=<SIZE>      Set code size limit.
    --perm-size=<SIZE>      Set perm size limit.
";

#[derive(Debug)]
pub struct Args {
    pub arg_argument: Option<Vec<String>>,
    pub arg_file: Option<String>,

    pub emit_ast: Option<String>,
    pub output: Option<String>,
    pub emit_asm: Option<String>,
    pub emit_asm_boots: bool,
    pub emit_asm_file: bool,
    pub emit_graph: Option<String>,
    pub emit_bytecode: Option<String>,
    pub emit_bytecode_compiler: Option<String>,
    pub emit_bytecode_boots: bool,
    pub emit_compiler: bool,
    pub emit_stubs: bool,
    pub enable_perf: bool,
    pub omit_bounds_check: bool,
    pub always_boots: bool,
    pub use_boots: Option<String>,
    pub version: bool,
    pub help: bool,
    pub emit_debug: Option<String>,
    pub emit_debug_native: bool,
    pub emit_debug_compile: bool,
    pub emit_debug_boots: bool,
    pub emit_debug_entry: bool,

    pub check: bool,
    pub disable_tlab: bool,
    pub disable_barrier: bool,
    pub test_filter: Option<String>,
    pub packages: Vec<(String, PathBuf)>,

    pub command: Command,
}

impl Default for Args {
    fn default() -> Args {
        Args {
            arg_argument: None,
            arg_file: None,

            output: None,
            emit_ast: None,
            emit_asm: None,
            emit_asm_boots: false,
            emit_asm_file: false,
            emit_graph: None,
            emit_bytecode: None,
            emit_bytecode_compiler: None,
            emit_bytecode_boots: false,
            emit_compiler: false,
            emit_stubs: false,
            emit_debug: None,
            emit_debug_compile: false,
            emit_debug_native: false,
            emit_debug_boots: false,
            emit_debug_entry: false,
            enable_perf: false,
            omit_bounds_check: false,
            always_boots: false,
            use_boots: None,
            version: false,
            help: false,
            check: false,
            disable_tlab: false,
            disable_barrier: false,
            test_filter: None,
            packages: Vec::new(),

            command: Command::Run,
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Command {
    Run,
    Test,
    TestBoots,
    Build,
}

impl Command {
    #[allow(dead_code)]
    pub fn is_run(&self) -> bool {
        match self {
            Command::Run => true,
            _ => false,
        }
    }

    pub fn is_test(&self) -> bool {
        match self {
            Command::Test => true,
            _ => false,
        }
    }

    pub fn is_test_boots(&self) -> bool {
        match self {
            Command::TestBoots => true,
            _ => false,
        }
    }

    pub fn is_build(&self) -> bool {
        match self {
            Command::Build => true,
            _ => false,
        }
    }
}

pub fn parse_arguments() -> Result<Args, String> {
    let cli_arguments: Vec<String> = std::env::args().collect();
    let mut args: Args = Default::default();
    let mut idx = 1;

    while idx < cli_arguments.len() {
        let arg = &cli_arguments[idx];

        if arg == "test" && idx == 1 {
            args.command = Command::Test;
        } else if arg == "test-boots" && idx == 1 {
            args.command = Command::TestBoots;
        } else if arg == "build" && idx == 1 {
            args.command = Command::Build;
        } else if arg == "--version" || arg == "-v" {
            args.version = true;
        } else if arg == "--check" {
            args.check = true;
        } else if arg == "-h" || arg == "--help" {
            args.help = true;
        } else if arg.starts_with("--emit-ast=") {
            args.emit_ast = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-asm=") {
            args.emit_asm = Some(argument_value(arg).into());
        } else if arg == "--emit-asm-file" {
            args.emit_asm_file = true;
        } else if arg == "--emit-asm-boots" {
            args.emit_asm_boots = true;
        } else if arg.starts_with("--emit-graph=") {
            args.emit_graph = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-bytecode=") {
            args.emit_bytecode = Some(argument_value(arg).into());
        } else if arg.starts_with("--emit-bytecode-compiler") {
            args.emit_bytecode_compiler = Some("all".into());
        } else if arg.starts_with("--emit-bytecode-compiler=") {
            args.emit_bytecode_compiler = Some(argument_value(arg).into());
        } else if arg == "--emit-bytecode-boots" {
            args.emit_bytecode_boots = true;
        } else if arg == "--always-boots" {
            args.always_boots = true;
        } else if arg.starts_with("--use-boots=") {
            args.use_boots = Some(argument_value(arg).into());
        } else if arg == "--emit-stubs" {
            args.emit_stubs = true;
        } else if arg.starts_with("--emit-debug=") {
            args.emit_debug = Some(argument_value(arg).into());
        } else if arg == "--emit-compiler" {
            args.emit_compiler = true;
        } else if arg == "--emit-debug-boots" {
            args.emit_debug_boots = true;
        } else if arg == "--emit-debug-native" {
            args.emit_debug_native = true;
        } else if arg == "--emit-debug-compile" {
            args.emit_debug_compile = true;
        } else if arg == "--emit-debug-entry" {
            args.emit_debug_entry = true;
        } else if arg == "--omit-bounds-check" {
            args.omit_bounds_check = true;
        } else if arg == "--enable-perf" {
            args.enable_perf = true;
        } else if arg.starts_with("--test-filter=") {
            args.test_filter = Some(argument_value(arg).into());
        } else if arg == "--disable-tlab" {
            args.disable_tlab = true;
        } else if arg == "-o" {
            if idx + 1 >= cli_arguments.len() {
                return Err("-o needs argument".into());
            }
            args.output = Some(cli_arguments[idx + 1].clone());
            idx += 1;
        } else if arg == "--disable-barrier" {
            args.disable_barrier = true;
        } else if arg == "--package" {
            if idx + 2 >= cli_arguments.len() {
                return Err("--package needs two arguments".into());
            }

            let name = cli_arguments[idx + 1].clone();
            let path = cli_arguments[idx + 2].clone();
            let path = PathBuf::from(path);

            args.packages.push((name, path));
            idx += 2;
        } else if arg.starts_with("-") {
            return Err(format!("unknown flag {}", arg));
        } else if args.arg_file.is_none() {
            args.arg_file = Some(arg.clone());

            // In `run` mode all arguments after the input file are arguments
            // for the program.
            if args.command.is_run() {
                let count = cli_arguments.len() - idx - 1;
                let mut arguments: Vec<String> = Vec::with_capacity(count);
                for arg in &cli_arguments[idx + 1..] {
                    arguments.push(arg.clone());
                }
                args.arg_argument = Some(arguments);
                break;
            }
        } else {
            return Err(format!("only one input file expected"));
        }

        idx = idx + 1;
    }

    Ok(args)
}

fn argument_value(arg: &str) -> &str {
    let idx = arg.find("=").expect("missing =");
    let (_, rhs) = arg.split_at(idx);
    &rhs[1..]
}

fn argument_usize(arg: &str) -> Result<usize, String> {
    let idx = arg.find("=").expect("missing =");
    let (name, value) = arg.split_at(idx);
    let value = &value[1..];
    match value.parse::<usize>() {
        Ok(value) => Ok(value),
        Err(_) => Err(format!("{}: invalid value '{}'", name, value)),
    }
}

pub fn print_help() {
    println!("{}", USAGE);
}
