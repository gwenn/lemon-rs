%token_type { i32 }

// The generated parser function takes a 4th argument as follows:
%extra_argument {result: Result}

%left PLUS MINUS.
%left DIVIDE TIMES.

%include {
#[macro_use]
extern crate log;

pub struct Result {
	expr: Option<Expr>,
}

#[derive(Debug)]
pub enum Operator {
    Add,
    Substract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub enum Expr {
    Number(i32),
    Binary(Operator, Box<Expr>, Box<Expr>),
}
impl Expr {
	fn binary(op: Operator, lhs: Expr, rhs: Expr) -> Expr {
		Expr::Binary(op, Box::new(lhs), Box::new(rhs))
	}
}
}

%syntax_error {
  panic!("near token {}: syntax error", yyminor);
}

program ::= expr(A). { self.result.expr = A; }

%type expr { Expr }
expr(A) ::= expr(B) MINUS expr(C). { A = Expr::binary(Operator::Substract, B, C); }
expr(A) ::= expr(B) PLUS expr(C). { A = Expr::binary(Operator::Add, B, C); }
expr(A) ::= expr(B) TIMES expr(C). { A = Expr::binary(Operator::Multiply, B, C); }
expr(A) ::= expr(B) DIVIDE expr(C). { A = Expr::binary(Operator::Divide, B, C); }

expr(A) ::= INTEGER(B). { A = Expr::Number(B); }
