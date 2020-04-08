# minic

Compiler for a small subset of the C programming language that uses LLVM IR. Written in Haskell.

This is not meant to be an actual compiler but is done as a learning experiment.

## Features

These are the current features of the compiler:

* Arithmetic
* If else statements
* Block scopes
* While statements
* Functions (partly)
* Function parameters
* Variable declarations, assignments
* boolean comparisons "<", ">", "=="
* types: int, void

Functions compile fine, still needs work on linking and calling instead of just defining. 

More functionality will be added based on what I find interesting and / or worth learning.
