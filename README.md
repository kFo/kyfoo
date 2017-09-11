# About
Kyfoo is an experimental programming language that is being developed as a personal research project. The language itself is far from complete, and the compiler even more so. Read: users should not rely on this compiler.

Kyfoo is compiled, statically typed, and far from complete. Did I mention that kyfoo is not complete? Since the future state of the language is unknown, you are welcome to read the philosophy page to get a general idea of what to expect. At this stage in development it is expected that any contributors are those interested in language design, compiler design, or general compiler tinkering/hacking. Users seeking "the next big language" are advised to move along, possibly to [Nim](https://nim-lang.org), [D](http://dlang.org), [Rust](https://www.rust-lang.org), or [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php). Current kyfoo example programs are available in the test directory of this repository to showcase its syntax.

# Building
## Requirements
- [LLVM 5.0](http://releases.llvm.org/download.html#5.0.0)
- [Visual Studio 2017](https://www.visualstudio.com/downloads)

## Instructions
1. [Build LLVM 5.0](http://llvm.org/docs/CMake.html).
2. Change vc15/local.props properties *LLVM_LIB_PATH* and *LLVM_INCLUDE_PATH* to point to your local LLVM lib and include directories.
3. Build with vc15/kyfoo.sln solution from Visual Studio 2017.

# Development
Kyfoo is in flux with many deferred decisions. Read the philosophy page for a general understanding of what to expect from the language.

# License
This is the repository for the kyfoo C++ reference compiler, written by Kyle Foley. It is licensed under the Boost Software License 1.0. See LICENSE.txt.
