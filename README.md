# minic

Compiler for a small subset of the C programming language that uses [Megaparsec](https://github.com/mrkkrp/megaparsec) for parsing
and [llvm-hs](https://github.com/llvm-hs/llvm-hs) for code generation.

## Goals

This compiler was written for a university compiler construction course and
as a learning experiment. The compiler is written in a fairly straightforward way and
can be of use to someone trying to write a compiler using the modern Haskell LLVM bindings.

## Features

Current features of the compiler include:

* Functions
* Parameters, return statements
* Recursion
* Command line arguments for main function
* If Else statements
* While statements
* Line (//) and block (/\*\*/) comments
* Globals
* Variable declarations, assignments
* Variable scopes
* Arrays (one-dimensional assignments, multidimensional parsing and declarations)
* boolean comparison operators: "<", ">", "=="
* Arithmetic operators: +, -, \*, /
* types: int, char, void
* pointer declarations
* Simple non-c print statements for ints

## Building

You will need llvm installed. For installation instructions check e.g:
<https://github.com/llvm-hs/llvm-hs/blob/llvm-9/README.md#installing-llvm>.

There is no linker included so you will need e.g. gcc to link object files.

To build with stack:

```bash
$ stack build
```

To run tests:

```bash
$ stack test
```

## Running

1. Run the compiler with an example file to produce an object file 'minic':
```bash
$ stack exec minic-exe /path/to/file
```
2. Link and compile the generated object file with e.g gcc:
```bash
$ gcc runtime.c minic
```

The compiler can also be run as a REPL for debugging.
It outputs the LLVM representation of the program on the screen
but does not compile to a file.

To run REPL:
```bash
$ stack exec minic-exe
```

## Examples

Example files to compile can be found in the examples folder. Files ending with .c are valid C programs,
files ending with .mc are not valid C as they contain non-c print statements.

## Print statements

Prints expressions that eval to ints.

grammar: 'print' '(' expr ')'

These are mainly just to get easy information of programs while running.
Functionality is provided by a C function that is in "runtime.c" and linked together with
the output of minic.

