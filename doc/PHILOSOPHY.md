# Introduction
This document attempts to characterize the philosophy and value system behind the kyfoo programming language. With the language
largely incomplete, this gives lurkers insight into whether to "pass" on kyfoo and pursue another language. It also helps guide any contributors in what contributions are likely to be accepted into the reference implementation. This document and language are intended to evolve over time. I will do my best to keep it organized and coherent, but read it more as a brain dump.

# Static vs Dynamic
Kyfoo is a statically typed language. The core value for this decision is the desire to catch errors at compile-time before they have a chance to reach production systems. We value the addage "If it compiles, it works."

In any decision of static vs dynamic, we favor static. The guiding principle is that static implies compile-time overhead as opposed to run-time overhead. Dynamic capabilities are be constructed from static ones, following the optional run-time overhead core value.

Kyfoo programs are to be thought of as proofs. The static checker evaluates all of the semantic rules of the language to decide whether a kyfoo program is well formed. This is analogous to checking one's deductive logic in a proof.

# Consistency
Any language designer desires to make a programming language "consistent." In kyfoo, built-in constructs (intrinsics) are modeled using the same features used for crafting user-defined data types. When deciding whether a feature is implemented in the compiler vs library, kyfoo prefers to implement it in a library. Kyfoo prefers to have more complex features emerge from simpler ones. Kyfoo prefers composition.

# Memory Model
Kyfoo does not provide automatic memory management beyond the call stack. Objects are allocated on the stack by default. Dynamic memory in kyfoo is done through pointers and reference types that refer to memory allocated by allocators available as libraries. The details of reference types are yet to be determined. E.g. what degree of alias analysis can be performed with reference types in kyfoo.

# Language Complexity
A balance must be struck between ease of compiler implementation and expressive modelling power in the language. In general, Kyfoo prefers expressive modelling power. We will gladly take on some implementation complexity to obtain significant gains in language power. We also simultaneously value orthogonal feature design, as stated previously (prefer composition of other existing features). In a contest between getting a fish and learning to fish, in Kyfoo we prefer learning to fish.
