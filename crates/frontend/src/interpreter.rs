use std::cell::RefCell;
use std::rc::Rc;

use colored::Colorize;
use thiserror::Error;

use crate::environment::{Env, EnvWrapper};
use crate::expr::{
    AssignExpr, BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr,
    StrLiteralExpr, UnaryExpr, VisitExpr,
};
use crate::results::{PhyReport, PhyResult};
use crate::stmt::{BlockStmt, ExprStmt, PrintStmt, Stmt, VarDeclStmt, VisitStmt};
use crate::values::{RtVal, RtValKind};

// ----------------
// Error managment
// ----------------
#[derive(Debug, Error, PartialEq)]
pub enum InterpErr {
    // Binop
    #[error("{0}")]
    OperationEvaluation(String),

    // Negate
    #[error("can't use '!' token on anything other than a bool value")]
    BangOpOnNonBool,

    #[error("can't use '-' token on anything other than an int or a real value")]
    NegateNonNumeric,

    #[error("{0}")]
    Negation(String),

    // Variables
    #[error("{0}")]
    VarDeclEnv(String),

    #[error("{0}")]
    GetVarEnv(String),

    #[error("{0}")]
    AssignEnv(String),

    #[error("uninitialized variable")]
    UninitializedValue,
}

impl PhyReport for InterpErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Interpreter error:".red(), self)
    }
}

pub(crate) type PhyResInterp = PhyResult<InterpErr>;
pub(crate) type InterpRes = Result<RtVal, PhyResInterp>;

// --------------
//  Interpreting
// --------------
#[derive(Default)]
pub struct Interpreter {
    // In RefCell because visitor methods only borrow (&) so we must be
    // able to mutate thanks to RefCell
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn interpret(&self, nodes: &Vec<Stmt>) -> InterpRes  {
        let mut res: RtVal = RtVal::new_null();

        for node in nodes {
            match node.accept(self, self.env.clone()) {
                Ok(r) => res = r,
                Err(e) => return Err(e),
            }
        }

        Ok(res)
    }
}

impl VisitStmt<RtVal, InterpErr> for Interpreter {
    fn visit_expr_stmt(&self, stmt: &ExprStmt, env: EnvWrapper) -> InterpRes {
        stmt.expr.accept(self, env)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt, env: EnvWrapper) -> InterpRes {
        let value = stmt.expr.accept(self, env)?;
        println!("{}", value);

        Ok(RtVal::new_null())
    }
    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt, env: EnvWrapper) -> InterpRes {
        let value = match &stmt.value {
            Some(v) => v.accept(self, env.clone())?,
            None => RtVal::new_null(),
        };

        env
            .borrow_mut()
            .declare_var(stmt.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::VarDeclEnv(e.to_string()), Some(stmt.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    // TODO:
    // TEST: test sur l'execution de block
    //       test aussi sur l'ssignement de variables dans les environnements tmp
    fn visit_block_stmt(&self, stmt: &BlockStmt, env: EnvWrapper) -> InterpRes {
        let tmp_env = Rc::new(RefCell::new(Env::new(Some(env.clone()))));

        for s in &stmt.stmts {
            let _ = s.accept(self, tmp_env.clone())?;
        }

        Ok(RtVal::new_null())
    }
}

// TODO:
// TEST: sur les variables non init
impl VisitExpr<RtVal, InterpErr> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr, env: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        let lhs = expr.left.accept(self, env.clone())?;
        if lhs == RtVal::new_null() {
            return Err(PhyResult::new(InterpErr::UninitializedValue, Some(expr.left.get_loc())))
        }

        let rhs = expr.right.accept(self, env)?;
        if rhs == RtVal::new_null() {
            return Err(PhyResult::new(InterpErr::UninitializedValue, Some(expr.right.get_loc())))
        }

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::new(
                InterpErr::OperationEvaluation(e.to_string()),
                Some(expr.loc.clone()),
            )),
        }
    }

    fn visit_assign_expr(&self, expr: &AssignExpr, env: EnvWrapper) -> InterpRes {
        let value = expr.value.accept(self, env.clone())?;
        env
            .borrow_mut()
            .assign(expr.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr, env: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        expr.expr.accept(self, env)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr, _: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr, _: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr, _: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr, env: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RtVal::new_null()),
            _ => env.borrow().get_var(expr.name.clone()).map_err(|e| {
                PhyResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
            }),
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr, env: EnvWrapper) -> Result<RtVal, PhyResInterp> {
        let value = expr.right.accept(self, env)?;

        match (&value.value, expr.operator.as_str()) {
            (RtValKind::IntVal(..) | RtValKind::RealVal(..), "!") => {
                return Err(PhyResult::new(
                    InterpErr::BangOpOnNonBool,
                    Some(expr.loc.clone()),
                ))
            }
            (RtValKind::BoolVal(..) | RtValKind::StrVal(..) | RtValKind::Null, "-") => {
                return Err(PhyResult::new(
                    InterpErr::NegateNonNumeric,
                    Some(expr.loc.clone()),
                ))
            }
            _ => {}
        }

        value.negate().map_err(|e| {
            PhyResult::new(InterpErr::Negation(e.to_string()), Some(expr.loc.clone()))
        })?;

        Ok(value)
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::{interpreter::InterpErr, utils::lex_parse_interp};

    #[test]
    fn interp_literals() {
        let code = "1";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "-45.";
        assert_eq!(lex_parse_interp(code).unwrap(), (-45f64).into());

        let code = "\"hello world!\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("hello world!").into()
        );
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
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "4 * \"foo\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "\"foo\" + \" \" + \"bar\"";
        assert_eq!(
            lex_parse_interp(code).unwrap(),
            EcoString::from("foo bar").into()
        );

        // Errors
        let code = "\"foo\" * 3.5";
        matches!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation { .. }
        );

        let code = "\"foo\" + 56";
        matches!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation { .. }
        );
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
        let code = "- \"foo\"";
        assert_eq!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::NegateNonNumeric
        );

        let code = "!8";
        assert_eq!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::BangOpOnNonBool
        );
    }
}
