[LEMON parser generator](https://www.sqlite.org/src/doc/trunk/doc/lemon.html) modified to generate Rust code.

## Unsupported

### Unsupported Grammar syntax

* `%token_destructor`: Code to execute to destroy token data
* `%default_destructor`: Code for the default non-terminal destructor
* `%destructor`: Code which executes whenever this symbol is
  popped from the stack during error processing

https://www.codeproject.com/Articles/1056460/Generating-a-High-Speed-Parser-Part-Lemon

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