# Introduction
How can the stack-based paradigm be made more modern, more readable, more expressive, and more productive?  The [Prowl Language](https://github.com/UberPyro/prowl) sets out to answer this question, taking influences from functional programing and other [concatenative languages](https://concatenative.org/wiki/view/Concatenative%20language) to take the paradigm to the next level.

Prowl has 3 major influences that you should know about:

- [The Kitten Language](http://kittenlang.org/)

- [The Vinegar Language](https://github.com/catseye/Vinegar)

- [Oniguruma Regex](https://github.com/kkos/oniguruma/blob/master/doc/RE)

The last one may come as a surprise to you, but indeed, Prowl is a stack-based language that uses *regex for control flow*. How is this possible? It all starts with the innovation made by Vinegar:

- All operations can fail

In a concatenative language, all *words* are typically thought of as functions `Stack -> Stack` (or equivalently, as a stack effect). However, if we allow functions that can fail, this changes to `Stack -> Maybe Stack`, creating a failure state we can match on. Consequently, we use alternation `|` as the means of handling: if `a` fails in `a | b`, then b is run.

While this alone helps to provide a better structure to the existing stack-based paradigm, we can do even better. It turns out that concatenation and alternation of programs corresponds almost exactly to concatenation and alternation of regexes, including the quantifiers derived from them. `a?` attempts to run `a` but does nothing if it fails, `b*` will keep running b *until* it fails; we can repurpose a lot of the syntax and work done on regexes to provide a more expressive control flow for our language.

In addition to this extension of the paradigm, we take great inspiration from the spirit of Kitten in designing the language. We use a clean syntax inspired greatly from MLs to help create more readable structures; we use expressive elements such as infix, binding expressions, and binding operators to create an experience less affected by the workings of the stack machine and enable more expressive styles. We plan to introduce [ML-style modules](https://people.mpi-sws.org/~rossberg/mixml/mixml-toplas.pdf) as a means of abstraction and [modular typeclasses](https://people.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf) to achieve polymorphism.

While the language is still in very early stages, an LLVM backend is planned for high-performance programming, and systems features are planned.  Its unique execution model allows us to escape the concept of function calls entirely – the regex of programs can compile to a DFA, allowing state transitions to become gotos and forgoing the need for a call stack or return addresses. The language is still TC as (heap-allocated) data stacks can be used to carry out general recursion. 
