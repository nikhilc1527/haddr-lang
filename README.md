# Haddr-Lang

## Purpose
This is a language that I am making, entirely in haskell trying to follow conventions of functional programming as closely as I can. As for the language itself, I want to make it a perfect blend between functional and imperative programming, allowing for an easy transition for imperative programmers into the functional style.

If you see some problems, please don't hesitate to submit a pull request!

## Feature Todo List

### Short Term
- [ ] update the readme
- [ ] make proper grammar specification
- [x] string/char literals
- [ ] type check proc call
- [ ] add type for function pointer
- [ ] import statement (copy paste style)
- [ ] lambdas (not closures)
- [ ] static linking (get rid of libc)
       make list of syscalls, std library
- [ ] improve ui
       add flag to run vs compile (-c, -r)
       output directory/files
- [ ] tuples

### Long Term
- [ ] proper testing
- [ ] move to stack and ditch cabal
- [ ] structs
       figure out how to pass structs to functions in ABI
- [ ] tagged union
- [ ] closures (maybe)
- [ ] modules
       parse header file to make pseudo-module for c ffi
- [ ] implicit currying functions

### Super Long Term
- [ ] monads
- [ ] self hosting
