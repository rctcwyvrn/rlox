rlox
---

A rust implementation of the bytecode VM from [crafting interpreters](https://craftinginterpreters.com/).

Considering this was my first project in rust and it was translating from a language I'm not 100% familiar with, I'm very happy with the final result. 

rlox is fast and reasonably well structured. Some things that were difficult for clox to implement basically dropped out for free because of my design (superclasses), but some things were a nightmare to implement due to not knowing this sort of structure would be needed (closures).

Overall a very fun experience and a nice project that I feel quite proud of

Todo:
-  Clean up all the fixme and todos in the code
-  Set up a proper test suite for rlox using the craftinginterpreters test harness
-  Bench rlox against clox
-  Start profiling and doing optimizations

Main differences between my rlox and clox
---
1. Functions are compiled into FunctionChunks that live on a vector in the compiler, runtime references to them are LoxFunction opcodes that hold an index into that vector
2. Same idea for classes with ClassChunks. Due to this we can inherit superclasses at compile time instead of runtime
3. Variable resolution was moved into it's own struct, Resolver, to manage the recursive nature of it. It doesn't 100% work to Lox spec, but it works for simple closures
4. For both the Resolver and the Compiler, the linked-list structure of Compilers in clox is replaced with a vector acting as a stack of compilers, and a few "currently targetting this" struct values and functions.
5. Only closures and class instances are "heap" allocated. The other values (double, string, bool) live on the rust stack and are automatically dropped by the rust runtime when they leave the lox vm stack. The heap allocated values use a "pointer" value which indexes into the "heap", which is just a vector that gets shuffled around a bit
6. The gc ended up being quite different due to my choice of a vector to hold the HeapObj values, but it still implements mark-sweep and it works :).
7. The VM is split into two parts, one that holds all the compilation information and reads off instructions from it (VM) and one that holds the stack and the heap and is responsible for all the execution state (VMState)
