# minic

Compiler for a small subset of the C programming language that uses [Megaparsec](https://github.com/mrkkrp/megaparsec) for parsing
and [llvm-hs](https://github.com/llvm-hs/llvm-hs) for code generation.

## Goals

This compiler was written for a university compiler construction course and further expanded
as a learning experiment. The compiler is written in a fairly straightforward way, and as
such might be of use to someone trying to write a compiler using the modern Haskell LLVM bindings.

## Features

Current features of the compiler include:

* Functions
* If else statements
* While statements
* Variable declarations, assignments
* Variable scopes
* boolean comparison operators: "<", ">", "=="
* Arithmetic operators: +, -, \*, /
* types: int, char, void
* pointers
* Simple non-c conformant print statements for ints

## Building

You will need llvm installed. For installation instructions check e.g:
<https://github.com/llvm-hs/llvm-hs/blob/llvm-9/README.md#installing-llvm>.

To build with stack:

```bash
$ stack build
```

To run tests:

```$ stack test ```

## Running

You can run the compiler with a single script (requires gcc):

```bash
$ ./minic $FILENAME
```

Or compile in steps:

1. Run the compiler with an example file to produce an object file:
```bash
$ stack exec minic-exe examples/$EXAMPLEFILE
```
2. Link and compile the generated object file with e.g gcc:
```bash
$ gcc runtime.c a.out -o $FILENAME
```

