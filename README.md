[![Build Status](https://github.com/gwenn/lemon-rs/workflows/CI/badge.svg)](https://github.com/gwenn/lemon-rs/actions)
[![Latest Version](https://img.shields.io/crates/v/sqlite3-parser.svg)](https://crates.io/crates/sqlite3-parser)
[![Docs](https://docs.rs/sqlite3-parser/badge.svg)](https://docs.rs/sqlite3-parser)
[![dependency status](https://deps.rs/repo/github/gwenn/lemon-rs/status.svg)](https://deps.rs/repo/github/gwenn/lemon-rs)

[LEMON parser generator](https://www.sqlite.org/src/doc/trunk/doc/lemon.html) modified to generate Rust code.

Lemon source and SQLite3 grammar were last synced as of April 2025.

## Unsupported

### Unsupported Grammar syntax

* `%token_destructor`: Code to execute to destroy token data
* `%default_destructor`: Code for the default non-terminal destructor
* `%destructor`: Code which executes whenever this symbol is
  popped from the stack during error processing

https://www.codeproject.com/Articles/1056460/Generating-a-High-Speed-Parser-Part-Lemon
https://www.sqlite.org/lemon.html

### SQLite

[SQLite lexer](http://www.sqlite.org/src/artifact?ci=trunk&filename=src/tokenize.c) and [SQLite parser](http://www.sqlite.org/src/artifact?ci=trunk&filename=src/parse.y) have been ported from C to Rust.
The parser generates an AST.

Lexer/Parser:
  - Keep track of position (line, column).
  - Streamable (stop at the end of statement).
  - Resumable (restart after the end of statement).

Lexer and parser have been tested with the following scripts:
  * https://github.com/bkiers/sqlite-parser/tree/master/src/test/resources
  * https://github.com/codeschool/sqlite-parser/tree/master/test/sql/official-suite which can be updated with script in https://github.com/codeschool/sqlite-parser/tree/master/test/misc

TODO:
  - [ ] Check generated AST (reparse/reinject)
  - [ ] [If a keyword in double quotes is used in a context where it cannot be resolved to an identifier but where a string literal is allowed, then the token is understood to be a string literal instead of an identifier.](https://sqlite.org/lang_keywords.html)
  - [ ] Tests
  - [ ] Do not panic while parsing
  - [x] CREATE VIRTUAL TABLE args
  - [ ] Zero copy (at least tokens)

### Unsupported by Rust

* `#line` directive

## API change

* No `ParseAlloc`/`ParseFree` anymore

## Features not tested

* NDEBUG
* YYNOERRORRECOVERY
* YYERRORSYMBOL

## To be fixed

* RHS are moved. Maybe it is not a problem if they are always used once.
  Just add a check in lemon...
* `%extra_argument` is not supported.

## Raison d'être

* [lemon_rust](https://github.com/rodrigorc/lemon_rust) does the same thing
but with an old version of `lemon`. And it seems not possible to use `yystack`
as a stack because items may be access randomly and the `top+1` item can be used.

* [lalrpop](https://github.com/nikomatsakis/lalrpop) would be the perfect
alternative but it does not support fallback/streaming
(see [this](https://github.com/nikomatsakis/lalrpop/issues/156) issue)
and compilation/generation is slow.

## Minimum supported Rust version (MSRV)

Latest stable Rust version at the time of release. It might compile with older versions.

