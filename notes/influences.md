The topos compiler and the language design would be exist without the prior work of others. During the development of topos compiler I had read many other projects and here I list down the notable feature of others.

### Haskell

- **Non-strict evaluation**, it only computes when neccessary, and lots of compiler optimizations focusing on making it fast. Which is ...
- **STG**, *spineless tagless graph* reduction machine. It allows potentially out-of-order optimization being made, and implement non-strict semantics, which makes the Haskell language 'lazy'.

- **Rewrite rules**, because of the lazy evaluation and the precense of STG IR, it allows programmer to specific a set of rules that will do aggressive expression rewrite during runtime.

As far as I know, [Scaba Hruska](https://twitter.com/csaba_hruska) is working on whole program optimization and strict functional languages backend, [GRIN compiler](https://grin-compiler.github.io/).

### Sixty / Sixten

- **Command based programming / Query based compiler architectures**. It used a dependent hashmap to store commands and dispatch on-demand. By implementing this in a compiler, recompiling a project after having made a few changes we only recompile what is affected by the changes

### Idris / Idris 2

- **Linear types**, it used the Quantitative Type Theory.

- **Optional totality check**, it provide an escape hatch for developers, making dependent typed programming practical.

- **Multiple backend**, idris recently got ported into *Racket* which runs on *Chez Scheme*.

### Setoidtt (previously setoidtt-proto)

- **Dependent types**

### Cyclone

- **Region-based memory management**, it replaces the use of garbage-collected memory with faster, scope-nested memory regions (arenas).

- **Linear/affine types**, an experimental application of the linear logic (substructal logic). So we can get safe, deterministic memory management without run-time GC bookkeeping costs.

> Which also leads to the Rust's lifetime, another explicit region annotations for low level memory management strategies.

### Rust

- **unsafe** annotation, and a comprehensive handbook disccussing the black magic of compiler. Allowing programmers to write safe, performant logic which the compiler cannot verify as safe.

- **Traits**, polymorphic dispatch.

###### Reference
* [The Facinating Influence of Cyclone](https://pling.jondgoodwin.com/post/cyclone/)
* [The Achitecture of Open Source Applications - The GHC](http://aosabook.org/en/ghc.html) - although the GHC had changed so much since that, it is still fun to read, to know how complex GHC is.
* [Lazy evaluation illustrated for Haskell divers](https://takenobu-hs.github.io/downloads/haskell_lazy_evaluation.pdf)
* [Query-based compiler architectures](https://ollef.github.io/blog/posts/query-based-compilers.html)
