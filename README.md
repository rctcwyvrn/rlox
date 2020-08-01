rlox
---

A rust implementation of the bytecode VM from [crafting interpreters](https://craftinginterpreters.com/).

Considering this was my first project in rust and it was translating from a language I'm not 100% familiar with, I'm very happy with the final result. 

rlox is fast and reasonably well structured. Some things that were difficult for clox to implement basically dropped out for free because of my design (superclasses), but some things were a nightmare to implement due to not knowing this sort of structure would be needed (closures).

Overall a very fun experience and a nice projec that I feel quite proud of

Todo:
-  Clean up all the fixme and todos in the code
-  Set up a proper test suite for rlox using the craftinginterpreters test harness
-  Bench rlox against clox
-  Start profiling and doing optimizations