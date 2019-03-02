[LEMON parser generator](https://www.sqlite.org/src/doc/trunk/doc/lemon.html) modified to generate Rust code.

## Unsupported

### Unsupported Grammar syntax

* `%token_destructor`: Code to execute to destroy token data
* `%default_destructor`: Code for the default non-terminal destructor
* `%destructor`: Code which executes whenever this symbol is
  popped from the stack during error processing

https://www.codeproject.com/Articles/1056460/Generating-a-High-Speed-Parser-Part-Lemon
https://www.sqlite.org/lemon.html

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