# Ark Lang

This is the main source code repository for Ark. It contains the compiler, standard library, and documentation.

JIT-compiler for the programming language Ark implemented in Rust. Works on
Linux, Windows and macOS (x86\_64 and aarch64). Build with:

## Compilation & Testing

Install Rust stable with the help of [rustup.rs](http://rustup.rs). Ark uses
`cargo` for building:

```
# build in debug and release mode
cargo build && cargo build --release

# run all tests in debug and release mode (needs Ruby)
tools/test && tools/test-release # Linux and macOS
tools/test.bat && tools/test-release.bat # Windows
```

## Working on the standard library

The standard library (stdlib) is included into the `ark`-binary at compile
time. Changing the stdlib therefore requires recompiling Ark, even though the
stdlib is written in Ark. In order to avoid this recompilation when working on
the stdlib, simply pass your working directory of the stdlib to Ark using the
`--stdlib` argument. With this parameter, Ark loads the stdlib from the
specified directory instead of the one bundled in the executable.