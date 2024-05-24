use std::rc::Rc;
use crate::lexer::Token;


enum Expr {
    Binary(Rc<BinaryExpr>),
    Grouping(Rc<GroupingExpr>),
    Literal(Rc<LiteralExpr>),
    Unary(Rc<UnaryExpr>),
}


struct BinaryExpr {
	left: Expr,
	operator: Token,
	right: Expr,
}

struct GroupingExpr {
	expression: Expr,
}

struct LiteralExpr {
	value: String,
}

struct UnaryExpr {
	operator: Token,
	right: Expr,
}

