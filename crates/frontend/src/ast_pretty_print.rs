use crate::expr::{BinaryExpr, GroupingExpr, LiteralExpr, UnaryExpr, VisitExpr};
use crate::{
    expr::Expr,
    results::ArcResult,
};

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&self, expr: &Expr) -> Result<String, ArcResult> {
        expr.accept(self)
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> Result<String, ArcResult> {
        let mut final_str: String = format!("({}", name);

        for expr in exprs {
            final_str.push(' ');
            final_str.push_str(expr.accept(self)?.as_str());
        }

        final_str.push_str(")");

        Ok(final_str)
    }
}

impl VisitExpr<String> for AstPrinter {
	fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<String, ArcResult> {
        self.parenthesize(expr.operator.value.as_str(), &[&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<String, ArcResult> {
        self.parenthesize("group", &[&expr.expr])
    }

    fn visit_literal_expr(&self, expr: &LiteralExpr) -> Result<String, ArcResult> {
        Ok(expr.value.to_string())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<String, ArcResult> {
        self.parenthesize(expr.operator.value.as_str(), &[&expr.right])
    }
}


#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::{expr::Expr, lexer::{Loc, Token, TokenKind}};
    use crate::ast_pretty_print::AstPrinter;
    use crate::expr::{BinaryExpr, UnaryExpr, LiteralExpr, GroupingExpr};

    #[test]
    fn test_print() {
        let expr = Expr::Binary(BinaryExpr {
            left: Box::new(
                Expr::Unary(UnaryExpr {
                    operator: Token {
                        kind: TokenKind::Minus,
                        value: EcoString::from("-"),
                        loc: Loc::default(),
                    },
                    right: Box::new(Expr::Literal(LiteralExpr { value: EcoString::from("98") }))
                })
            ),
            operator: Token {
                kind: TokenKind::Star,
                value: EcoString::from("*"),
                loc: Loc::default()
            },
            right: Box::new(Expr::Grouping(GroupingExpr {
                expr: Box::new(Expr::Literal(LiteralExpr { value: EcoString::from("13.45") }))
            }))
        });
        
        let app = AstPrinter {};
        assert_eq!("(* (- 98) (group 13.45))", app.print(&expr).unwrap())
    }
}
