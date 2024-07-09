use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use tools::{
    results::{PhyReport, PhyResult},
    ToUuid,
};

use crate::callable::Callable;
use crate::environment::Env;
use crate::native_functions::{NativeClock, PhyNativeFn};
use crate::values::RtVal;
use frontend::ast::expr::{
    AssignExpr, BinaryExpr, CallExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr,
    RealLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr,
};
use frontend::ast::stmt::{
    BlockStmt, ExprStmt, FnDeclStmt, ForStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, VarDeclStmt,
    VisitStmt, WhileStmt,
};

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

    // If
    #[error("'if' condition is not a boolean")]
    NonBoolIfCond,

    // While
    #[error("'while' condition is not a boolean")]
    NonBoolWhileCond,

    // For
    #[error("{0}")]
    ForLoop(String),

    // Call
    #[error("only functions and structures are callable")]
    NonFnCall,

    #[error("wrong arguments number: expected {0} but got {1}")]
    WrongArgsNb(usize, usize),

    #[error("{0}")]
    FnCall(String),

    // Results
    #[error("return: {0}")]
    Return(RtVal),
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
pub struct Interpreter {
    pub globals: Rc<RefCell<Env>>,
    pub env: Rc<RefCell<Env>>,
    pub locals: HashMap<String, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Env::new(None)));

        let _ = globals.borrow_mut().declare_var(
            EcoString::from("clock"),
            RtVal::NativeFnVal(Rc::new(PhyNativeFn {
                name: EcoString::from("clock"),
                func: Rc::new(NativeClock),
            })),
        );

        let env = globals.clone();

        Self {
            globals,
            env,
            locals: HashMap::new(),
        }
    }
}

impl Interpreter {
    pub fn interpret(&mut self, nodes: &Vec<Stmt>, locals: HashMap<String, usize>) -> InterpRes {
        self.locals = locals;

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
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> InterpRes {
        stmt.expr.accept(self)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> InterpRes {
        let value = stmt.expr.accept(self)?;
        println!("{}", value);

        Ok(RtVal::new_null())
    }
    fn visit_var_decl_stmt(&mut self, stmt: &VarDeclStmt) -> InterpRes {
        let value = match &stmt.value {
            Some(v) => v.accept(self)?,
            None => RtVal::new_null(),
        };

        self.env
            .borrow_mut()
            .declare_var(stmt.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::VarDeclEnv(e.to_string()), Some(stmt.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> InterpRes {
        let new_env = Env::new(Some(self.env.clone()));
        self.execute_block_stmt(&stmt.stmts, new_env)?;

        Ok(RtVal::new_null())
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> InterpRes {
        let cond = stmt.condition.accept(self)?;

        match cond {
            RtVal::BoolVal(b) => match b.borrow().value {
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
            },
            _ => Err(PhyResult::new(
                InterpErr::NonBoolIfCond,
                Some(stmt.loc.clone()),
            )),
        }
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> InterpRes {
        loop {
            let cond = stmt.condition.accept(self)?;

            match cond {
                RtVal::BoolVal(b) => match b.borrow().value {
                    true => {
                        stmt.body.accept(self)?;
                    }
                    false => break,
                },
                _ => {
                    return Err(PhyResult::new(
                        InterpErr::NonBoolWhileCond,
                        Some(stmt.loc.clone()),
                    ))
                }
            }
        }

        Ok(RtVal::new_null())
    }

    fn visit_for_stmt(&mut self, stmt: &ForStmt) -> InterpRes {
        let new_env = Env::new(Some(self.env.clone()));
        let prev_env = std::mem::replace(&mut self.env, Rc::new(RefCell::new(new_env)));

        self.visit_var_decl_stmt(&stmt.placeholder)?;
        let mut range = 0..stmt.range.start;

        if let Some(i) = stmt.range.end {
            range = stmt.range.start..i;
        }

        for i in range {
            self.env
                .borrow_mut()
                .assign(stmt.placeholder.name.clone(), i.into())
                .map_err(|e| {
                    PhyResult::new(InterpErr::ForLoop(e.to_string()), Some(stmt.loc.clone()))
                })?;

            stmt.body.accept(self)?;
        }

        let _ = std::mem::replace(&mut self.env, prev_env);

        Ok(RtVal::new_null())
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> Result<RtVal, PhyResult<InterpErr>> {
        let func = RtVal::new_fn(stmt, self.env.clone());

        self.env
            .borrow_mut()
            .declare_var(stmt.name.clone(), func)
            .map_err(|_| {
                PhyResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Result<RtVal, PhyResult<InterpErr>> {
        let mut value = RtVal::new_null();

        if let Some(v) = &stmt.value {
            value = v.accept(self)?;
        }

        Err(PhyResult::new(InterpErr::Return(value), None))
    }
}

impl Interpreter {
    pub fn execute_block_stmt(&mut self, stmts: &Vec<Stmt>, env: Env) -> InterpRes {
        let prev_env = std::mem::replace(&mut self.env, Rc::new(RefCell::new(env)));

        let mut res = Ok(RtVal::new_null());
        for s in stmts {
            res = s.accept(self);

            if res.is_err() {
                break;
            }
        }

        let _ = std::mem::replace(&mut self.env, prev_env);

        res
    }
}

impl VisitExpr<RtVal, InterpErr> for Interpreter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> InterpRes {
        let lhs = expr.left.accept(self)?;
        if lhs == RtVal::new_null() {
            return Err(PhyResult::new(
                InterpErr::UninitializedValue,
                Some(expr.left.get_loc()),
            ));
        }

        let rhs = expr.right.accept(self)?;
        if rhs == RtVal::new_null() {
            return Err(PhyResult::new(
                InterpErr::UninitializedValue,
                Some(expr.right.get_loc()),
            ));
        }

        match lhs.operate(&rhs, &expr.operator) {
            Ok(res) => Ok(res),
            Err(e) => Err(PhyResult::new(
                InterpErr::OperationEvaluation(e.to_string()),
                Some(expr.loc.clone()),
            )),
        }
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> InterpRes {
        let value = expr.value.accept(self)?;

        self.env
            .borrow_mut()
            .assign(expr.name.clone(), value)
            .map_err(|e| {
                PhyResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.loc.clone()))
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> InterpRes {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> InterpRes {
        Ok(expr.value.into())
    }

    fn visit_real_literal_expr(&mut self, expr: &RealLiteralExpr) -> InterpRes {
        Ok(expr.value.into())
    }

    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> InterpRes {
        Ok(expr.value.clone().into())
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> InterpRes {
        match expr.name.as_str() {
            "true" => Ok(true.into()),
            "false" => Ok(false.into()),
            "null" => Ok(RtVal::new_null()),
            _ => match self.locals.get(&expr.to_uuid()) {
                Some(i) => self
                    .env
                    .borrow()
                    .get_var_at(expr.name.clone(), i)
                    .map_err(|e| {
                        PhyResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                    }),
                None => self.env.borrow().get_var(expr.name.clone()).map_err(|e| {
                    PhyResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                }),
            },
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> InterpRes {
        let value = expr.right.accept(self)?;

        match (&value, expr.operator.as_str()) {
            (RtVal::IntVal(..) | RtVal::RealVal(..), "!") => {
                return Err(PhyResult::new(
                    InterpErr::BangOpOnNonBool,
                    Some(expr.loc.clone()),
                ))
            }
            (RtVal::BoolVal(..) | RtVal::StrVal(..) | RtVal::Null, "-") => {
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

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> InterpRes {
        let left = expr.left.accept(self)?;
        let op = expr.operator.as_str();

        if op == "or" {
            match &left {
                RtVal::BoolVal(b) => {
                    if b.borrow().value {
                        return Ok(left);
                    }
                }
                _ => {
                    return Err(PhyResult::new(
                        InterpErr::NonBoolIfCond,
                        Some(expr.loc.clone()),
                    ))
                }
            }
        } else if op == "and" {
            match &left {
                RtVal::BoolVal(b) => {
                    if !b.borrow().value {
                        return Ok(left);
                    }
                }
                _ => {
                    return Err(PhyResult::new(
                        InterpErr::NonBoolIfCond,
                        Some(expr.loc.clone()),
                    ))
                }
            }
        }

        expr.right.accept(self)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> InterpRes {
        let callee = expr.callee.accept(self)?;

        let mut args: Vec<RtVal> = vec![];
        for a in &expr.args {
            args.push(a.accept(self)?);
        }

        if let RtVal::FuncVal(f) = callee {
            if f.arity() != args.len() {
                return Err(PhyResult::new(
                    InterpErr::WrongArgsNb(f.arity(), args.len()),
                    Some(expr.loc.clone()),
                ));
            }

            f.call(self, args).map_err(|e| {
                PhyResult::new(InterpErr::FnCall(e.err.to_string()), Some(expr.loc.clone()))
            })
        } else {
            Err(PhyResult::new(InterpErr::NonFnCall, Some(expr.loc.clone())))
        }
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
        assert_eq!(
            lex_parse_interp(code).err().unwrap().err,
            InterpErr::UninitializedValue
        );
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

    #[test]
    fn logical() {
        let code = "
var a = true
var b = 0
if a and b == 0 { b = 1 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = true
var b = 0
if a and 2 + 2 == 5 { } else { b = 1 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = true
var b = 0
if a or false { b = 1 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = false
var b = 0
if a or false {} else { b = 1 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 1.into());

        let code = "
var a = true
var b = 42
if a and b == 41 or b == 42 and false {} else { b = 45 }
b
";
        assert_eq!(lex_parse_interp(code).unwrap(), 45.into());
    }

    #[test]
    fn while_stmt() {
        let code = "
var a = 0
while a < 5 {
    a = a + 1
}
a
";
        assert_eq!(lex_parse_interp(code).unwrap(), 5.into());
    }

    #[test]
    fn for_stmt() {
        let code = "
var a = 0
for i in 5 { a = a + i }
a
";
        assert_eq!(lex_parse_interp(code).unwrap(), 10.into());

        let code = "
var a = 0
for i in 5..10 { a = a + i }
a
";
        assert_eq!(lex_parse_interp(code).unwrap(), 35.into());
    }

    #[test]
    fn functions() {
        let code = "
var res
fn add(a, b) {
    res = a + b
}
add(5, 6)
res
";
        assert_eq!(lex_parse_interp(code).unwrap(), 11.into());
    }

    #[test]
    fn first_class_fn() {
        let code = "
fn add(a, b) { return a+b }
var c = add
c(1, 2)
";
        assert_eq!(lex_parse_interp(code).unwrap(), 3.into());
    }

    #[test]
    fn recurs_and_break_fn() {
        let code = "
fn fib(n) {
    if n <= 1 { return n }

    return fib(n-2) + fib(n-1)
}

fib(20)
";
        assert_eq!(lex_parse_interp(code).unwrap(), 6765.into());
    }

    #[test]
    fn closure_env() {
        let code = "
fn makeCounter() {
  var i = 0
  fn count() {
    i = i + 1
    return i
  }

  return count
}

var counter = makeCounter()
var a = 0
a = counter()
a = counter()
a
";
        assert_eq!(lex_parse_interp(code).unwrap(), 2.into());
    }
}
