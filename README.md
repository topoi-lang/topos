# Topos

Topos is a learn-by-doing compiler project. This aims is to create a sustainable functional programming language to make programming more fun and more control, in a higher-level language manner.

### What I am trying to do
- [x] S expression parser in Haskell
- [ ] Parser that generate compiler workable AST
    - accumulate the definitions per source unit (define / defun / lambda)
    - simple untyped lambda calculus operations (lam / app / let / var)
- [ ] Typecheck monad
- [ ] Type inference
- [ ] Evaluator / Graph (Should I?)
    - which backend should I targeting? GRIN? wasm? cranelift JIT?

### Ideas
- Guarded Command Language
- Call By Push Value

### Resources
* [Topos knowledge bank (notion)](https://www.notion.so/Topos-07f4ed2a60234d458703fe416282dedf)
