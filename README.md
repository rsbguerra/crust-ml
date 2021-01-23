# Crust - A Pico-Rust Compiler

A compiler for a subset of the Rust language, the final project of the Programming Languages and Compilers course at the University of Beira Interior (UBI). Developed in OCaml with Menhir and OCamllex.


## How to Use It

This project uses a combination of dune and a makefile. To use the compiler there's only the need to use the makefile in the directory `/Compiler`.

```sh
make
```

This comand will generate the  `crust.exe` executable in the dune default directory (`Compiler/_build/default`).

This makefile offers a set of options:

-parser-only : Executes only the lexer and parser;
-print-ast   : Prints the abstract syntax tree;
-print-tast  : Prints the typed abstract syntax tree;
-print-past  : Prints the pre-compiled abstract syntax tree;
-test-s      : Runs the sample `/Compiler/tests/test.rs`;
-test        : Runs a collection of tests, specified in the `/Compiler/tests/test/run-tests.sh` script;
-clean       : Removes every generated file.

Using an option:

```sh
dune exec ./crust.exe -print-ast file.rs
```
or with the makefile

```sh
make print-past
```

## How to Test

This project contains a folder with a set of tests, the easiast way to test the compiler and interpreter is to use the entry `test` of the makefile.

```sh
make test
```

In the directory `/Compiler/tests` there's another **Readme** file about the test scripts developed to this project.

## Pico-Rust Documentation

The full documentation of the Pico-Rust language can be accessed bellow:

-[Pico-Rust Documentation](https://dario-santos.github.io/2021/01/15/crust.html)
