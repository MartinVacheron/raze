use std::cell::RefCell;
use std::rc::Rc;

use colored::Colorize;
use thiserror::Error;

use crate::environment::Env;
use crate::expr::{
    AssignExpr, BinaryExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, RealLiteralExpr,
    StrLiteralExpr, UnaryExpr, VisitExpr,
};
use crate::results::{PhyReport, PhyResult};
use crate::stmt::{BlockStmt, ExprStmt, IfStmt, PrintStmt, Stmt, VarDeclStmt, VisitStmt};
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

    #[error("'if' condition is not a boolean")]
    NonBoolIfCond,
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
    // able to mutate thanks to RefCell and it can be multiple owners
    env: RefCell<Rc<RefCell<Env>>>,
}

impl Interpreter {
    pub fn interpret(&self, nodes: &Vec<Stmt>) -> InterpRes  {
        let mut res: RtVal = RtVal::new_null();

        for node in nodes {
            match node.accept(self) {
                Ok(r) => res = r,
                Err(e) => return Err(e),
            }
        }

        Ok(res)
    }
}

impl VisitStmt<RtVal, InterpErr> for Interpreter {
    fn visit_expr_stmt(&self, stmt: &ExprStmt) -> InterpRes {
        stmt.expr.accept(self)
    }

    fn visit_print_stmt(&self, stmt: &PrintStmt) -> InterpRes {
        let value = stmt.expr.accept(self)?;
        println!("{}", value);

        Ok(RtVal::new_null())
    }
    fn visit_var_decl_stmt(&self, stmt: &VarDeclStmt) -> InterpRes {
        let value = match &stmt.value {
            Some(v) => v.accept(self)?,
            None => RtVal::new_null(),
        };

        self.env
            .borrow()
            .borrow_mut()
            .declare_var(stmt.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::VarDeclEnv(e.to_string()), Some(stmt.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_block_stmt(&self, stmt: &BlockStmt) -> InterpRes {
        let new_env = Env::new(Some(self.env.borrow().clone()));
        let prev_env = self.env.replace(Rc::new(RefCell::new(new_env)));

        println!("new env created successfuly!");
        for s in &stmt.stmts {
            let _ = s.accept(self)?;
        }

        self.env.replace(prev_env);

        Ok(RtVal::new_null())
    }

    fn visit_if_stmt(&self, stmt: &IfStmt) -> Result<RtVal, PhyResult<InterpErr>> {
        let cond = stmt.condition.accept(self)?;

        match cond.value {
            RtValKind::BoolVal(b) => match b.borrow().value {
                true => {
                    if let Some(t) = &stmt.then_branch {
                        t.accept(self)
                    } else {
                        Ok(RtVal::new_null())
                    }
                }
                false => {
                    if let Some(e) = &stmt.else_branch {
                        e.accept(self)
                    } else {
                        Ok(RtVal::new_null())
                    }
                }
            }
            _ => Err(PhyResult::new(InterpErr::NonBoolIfCond, Some(stmt.loc.clone())))
        }
    }
}

impl VisitExpr<RtVal, InterpErr> for Interpreter {
    fn visit_binary_expr(&self, expr: &BinaryExpr) -> Result<RtVal, PhyResInterp> {
        let lhs = expr.left.accept(self)?;
        if lhs == RtVal::new_null() {
            return Err(PhyResult::new(InterpErr::UninitializedValue, Some(expr.left.get_loc())))
        }

        let rhs = expr.right.accept(self)?;
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

    fn visit_assign_expr(&self, expr: &AssignExpr) -> InterpRes {
        let value = expr.value.accept(self)?;

        self.env
            .borrow()
            .borrow_mut()
            .assign(expr.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_grouping_expr(&self, expr: &GroupingExpr) -> Result<RtVal, PhyResInterp> {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&self, expr: &IntLiteralExpr) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&self, expr: &RealLiteralExpr) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&self, expr: &StrLiteralExpr) -> Result<RtVal, PhyResInterp> {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&self, expr: &IdentifierExpr) -> Result<RtVal, PhyResInterp> {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RtVal::new_null()),
            _ => self.env.borrow().borrow().get_var(expr.name.clone()).map_err(|e| {
                PhyResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
            }),
        }
    }

    fn visit_unary_expr(&self, expr: &UnaryExpr) -> Result<RtVal, PhyResInterp> {
        let value = expr.right.accept(self)?;

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

    #[test]
    fn variable() {
        let code = "var a = -8
a";
        assert_eq!(lex_parse_interp(code).unwrap(), (-8).into());

        let code = "var a = -8
a = 4 + a*2
a";
        assert_eq!(lex_parse_interp(code).unwrap(), (-12).into());

        // Errors
        let code = "a = 5";
        assert!(matches!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::AssignEnv { .. }
        ));

        let code = "var b
3 + b";
        assert_eq!(lex_parse_interp(code).err().unwrap().err, InterpErr::UninitializedValue);
    }

    #[test]
    fn block() {
        let code = "var a = -8
{
    var b = 1
    b = a + 9
    a = b
}
a";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());
    }

    #[test]
    fn if_stmt() {
        let code = "
var a = true
var b = 0
if a { b = 1 } else {}
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = false
var b = 0
if a { b = 8 } else { b = 1 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = false
var b = 42
if a {} else {}
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 42.into());

        // Errors
        let code = "
var a = 5
if a {} else {}
";
        assert!(lex_parse_interp(code).err().unwrap().err == InterpErr::NonBoolIfCond);
    }
}
