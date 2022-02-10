# Haddr-Lang

## Purpose
This is a language that I am making, entirely in haskell trying to follow conventions of functional programming as closely as I can. As for the language itself, I want to make it a perfect blend between functional and imperative programming, allowing for an easy transition for imperative programmers into the functional style.

If you see some problems, please don't hesitate to submit a pull request!

## Feature Todo List

### Short Term
- [ ] preprocessor macros
- [x] for loop
- [x] break/continue of loops
- [ ] update the readme
- [ ] make proper grammar specification
- [x] string/char literals
- [x] type check proc call
- [ ] add type for function pointer
- [ ] import statement (copy paste style)
- [ ] lambdas (not closures)
- [ ] static linking (get rid of libc), make list of syscalls, std library
- [ ] improve ui, add flag to run vs compile (-c, -r), output directory/files
- [ ] tuples
- [ ] different sized int/uint types
- [ ] make if statements/blocks into expressions
- [ ] global variables in (bss)
- [x] compile time constants, arrays initialized to size of compile time constant
- [ ] graphviz of control flow
- [ ] profiling
- [ ] ++, --, & (address of), * (dereference), +=, -= operators
- [ ] add emacs/vim support
- [ ] type check return statements
- [ ] do nothing if statement doesnt do anything

### Long Term
- [ ] generics (with typeclasses / type constraints)
- [ ] proper testing
- [ ] move to stack and ditch cabal
- [ ] structs - figure out how to pass structs to functions in ABI
- [ ] tagged union
- [ ] closures (maybe)
- [ ] modules
- [ ] parse c header file to make pseudo-module for c ffi
- [ ] implicit currying functions
- [ ] separate type checking into its own step (and make errors have source code row/column labels)
- [ ] type inference
- [ ] find mechanism to do RAII (destructors / defer)
- [ ] use block ([reference](https://youtu.be/QM1iUe6IofM?t=2502))
- [ ] static reflection

### Super Long Term
- [ ] lsp
- [ ] monads
- [ ] self hosting
- [ ] asm optimizations (using linear IR)
