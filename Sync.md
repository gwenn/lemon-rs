When some changes happen in the official SQLite repository,
they can be applied locally:
 - $SQLITE/tool/lemon.c => $RLEMON/third_party/lemon.c
 - $SQLITE/tool/lempar.c => $RLEMON/third_party/lempar.rs
 - $SQLITE/tool/mkkeywordhash.c => $RLEMON/dialect/src/lib.rs
 - $SQLITE/src/tokenize.c => $RLEMON/lexer/src/sql/mod.rs
 - $SQLITE/src/parse.y => $RLEMON/parser/src/parse.y (and $RLEMON/dialect/src/token.rs)
 - $SQLITE/src/parse.y => $RLEMON/parser/src/parse.y (and $RLEMON/dialect/src/token.rs)
