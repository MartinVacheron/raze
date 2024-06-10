use crate::expr::{
    BinaryExpr, Expr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr
};
use crate::values::RuntimeVal;
use crate::results::PhyResult;

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, nodes: &Vec<Expr>) -> Result<RuntimeVal, PhyResult> {
        let mut res: RuntimeVal = RuntimeVal::Null;

        for node in nodes {
           match node.accept(self) {
                Ok(r) => res = r,
                Err(e) => return Err(PhyResult::runtime_error(e.msg, node.get_loc()))
            }
        }

        Ok(res)
    }
}

impl VisitExpr<RuntimeVal> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<RuntimeVal, PhyResult> {
        let lhs = expr.left.accept(self)?;
        let rhs = expr.right.accept(self)?;

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::interpreter_error(e.msg, expr.loc.clone()))
        }
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<RuntimeVal, PhyResult> {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<RuntimeVal, PhyResult> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<RuntimeVal, PhyResult> {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RuntimeVal::new_null()),
            _ => todo!()
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<RuntimeVal, PhyResult> {
        let value = expr.right.accept(self)?;

        match (&value, expr.operator.as_str()) {
            (RuntimeVal::IntVal(..) | RuntimeVal::RealVal(..), "!") => return Err(PhyResult::interpreter_error(
                "Can't use '!' token on anything other than a bool value".into(),
                expr.loc.clone(),
            )),
            (RuntimeVal::BoolVal(..), "-") => return Err(PhyResult::interpreter_error(
                "Can't use '-' token on anything other than an int or a real value".into(),
                expr.loc.clone(),
            )),
            _ => {}
        }

        value.negate()?;

        Ok(value)
    }
}


#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::utils::{lex_parse_interp, produce_runtime_error};

    #[test]
    fn interp_literals() {
        let code = "1";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "-45.";
        assert_eq!(lex_parse_interp(code).unwrap(), (-45f64).into());

        let code = "\"hello world!\"";
        assert_eq!(lex_parse_interp(code).unwrap(), EcoString::from("hello world!").into());

    }

    #[test]
    fn interp_binop() {
        let code = "1 +2";
        assert_eq!(lex_parse_interp(code).unwrap(), 3.into());

        let code = "1. + -2 *24";
        assert_eq!(lex_parse_interp(code).unwrap(), (-47f64).into());

        let code = "5 + (6 * (2+3)) - (((6)))";
        assert_eq!(lex_parse_interp(code).unwrap(), 29.into());
    }

    #[test]
    fn interp_str_op() {
        let code = "\"foo\" * 4";
        assert_eq!(lex_parse_interp(code).unwrap(), EcoString::from("foofoofoofoo").into());

        let code = "4 * \"foo\"";
        assert_eq!(lex_parse_interp(code).unwrap(), EcoString::from("foofoofoofoo").into());

        let code = "\"foo\" + \" \" + \"bar\"";
        assert_eq!(lex_parse_interp(code).unwrap(), EcoString::from("foo bar").into());

        // Errors
        let code = "\"foo\" * 3.5";
        assert!(produce_runtime_error(&code));

        let code = "\"foo\" + 56";
        assert!(produce_runtime_error(&code));
    }
    
    #[test]
    fn interp_negation() {
        let code = "-3";
        assert_eq!(lex_parse_interp(code).unwrap(), (-3).into());

        let code = "-3.";
        assert_eq!(lex_parse_interp(code).unwrap(), (-3f64).into());

        let code = "!true";
        assert_eq!(lex_parse_interp(code).unwrap(), false.into());

        let code = "!false";
        assert_eq!(lex_parse_interp(code).unwrap(), true.into());

        // Errors
        let code = "--3";
        assert!(produce_runtime_error(&code));

        let code = "- \"foo\"";
        assert!(produce_runtime_error(&code));
    }
}
