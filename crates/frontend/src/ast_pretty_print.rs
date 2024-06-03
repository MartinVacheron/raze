use crate::expr::{
    BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::{expr::Expr, results::RazeResult};

pub struct AstPrinter {}

impl AstPrinter {
    pub fn print(&self, expr: &Expr) -> Result<String, RazeResult> {
        expr.accept(self)
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> Result<String, RazeResult> {
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
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<String, RazeResult> {
        self.parenthesize(expr.operator.as_str(), &[&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<String, RazeResult> {
        self.parenthesize("group", &[&expr.expr])
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<String, RazeResult> {
        Ok(format!("{}", expr.value))
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<String, RazeResult> {
        Ok(format!("{}", expr.value))
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<String, RazeResult> {
        Ok(format!("{}", expr.value))
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<String, RazeResult> {
        Ok(expr.name.to_string())
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<String, RazeResult> {
        self.parenthesize(expr.operator.as_str(), &[&expr.right])
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::ast_pretty_print::AstPrinter;
    use crate::expr::Expr;
    use crate::expr::{BinaryExpr, GroupingExpr, IntLiteralExpr, RealLiteralExpr, UnaryExpr};
    use crate::lexer::Loc;

    #[test]
    fn test_print() {
        let expr = Expr::Binary(BinaryExpr {
            left: Box::new(Expr::Unary(UnaryExpr {
                operator: EcoString::from("-"),
                right: Box::new(Expr::IntLiteral(IntLiteralExpr {
                    value: 98,
                    loc: Loc::new(0, 0),
                })),
                loc: Loc::new(0, 0)
            })),
            operator: EcoString::from("*"),
            right: Box::new(Expr::Grouping(GroupingExpr {
                expr: Box::new(Expr::RealLiteral(RealLiteralExpr {
                    value: 13.45,
                    loc: Loc::new(0, 0),
                })),
                loc: Loc::new(0, 0),
            })),
            loc: Loc::new(0, 0),
        });

        let app = AstPrinter {};
        assert_eq!("(* (- 98) (group 13.45))", app.print(&expr).unwrap())
    }
}
