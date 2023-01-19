# Zapper Web Assembly Interpreter
Zapper is a purely-functional interpreter for a subset of Web Assembly implemented in Elm.
This project was created as a one-semester independent study class for university, and is not intended for general use.
It currently supports a subset of the s-expression-based textual format of Web Assembly.
The most complex thing I have tested this on is a simple recursive factorial program.

### What works
The lexer, parser, type checker, and interpreter only support certain constructs of Web Assembly. This includes:
* Numeric data types (i32, i64, f32, f64) (currently implemented with Elm's Int and Float types, so they are not very precise).
* Algebraic operations
* Local variables
* Control flow instructions
* Module and function declarations
* Calling functions, recursion

### What doesn't work
I chose to not implement these due to time and complexity constraints.
* Vector and reference data types
* Table and memory instructions
* Bitwise operations
* Module export, import, and non-inline function type declarations
* global variables
* Referring to locals and scopes by index rather than identifier
* Unicode escape sequences

## Reflection
This project allowed me to try functional programming techniques in a medium-sized project and critically evaluate their effectiveness.
I wish to reflect on both my successes and what I could have done better given the same amount of time.

### What went well
Despite my professor's objections, I added an intermediate step between lexing and parsing that converted the token stream into s-expressions.
Though this choice was nonstandard, I believe this made the parser much more elegant and concise.
My data types and parser for the abstract syntax tree were elegant within the constraints of Elm and accurately represented the structure of Web Assembly.
I believe I made good use of unit testing throughout the project. Elm provides an elegant fuzz-testing library that I made frequent use of.

### What could have been better
I could have easily implemented much better error messages with only minor modifications to the token and abstract syntax tree types.
This not only would have improved the user experience but also would have made debugging much easier.
I implemented folded instructions as a separate datatype to allow them to show up if I printed the abstract syntax tree, but this added redundancies in the the type checker and interpreter.
Also compounding friction in the interpreter and type checker were bad naming practices, resulting in the two being somewhat convoluted.
The type checker is not as thoroughly-tested as I would have liked. If I had time, I also would have liked to make a frontend where one could copy and paste code, run it, and see its output.

## Looking Forward
This was an interesting experiment.
It allowed me to experience firsthand how functional programming makes code very reliable and easy to test, but also can make things unreasonably obtuse and lead to over-abstraction.
I don't see myself implementing a compiler or interpreter in a purely functional language in the future due to both code quality and performance concerns, but I enjoyed making this.
