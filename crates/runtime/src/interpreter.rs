use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use colored::Colorize;
use ecow::EcoString;
use frontend::ast::expr::{GetExpr, SelfExpr, SetExpr};
use thiserror::Error;
use tools::results::Loc;
use tools::results::{RevReport, RevResult};

use crate::callable::Callable;
use crate::environment::Env;
use crate::native_functions::RevNativeFn;
use crate::values::{Function, RtVal, Negate};
use crate::native_functions::NativeFnErr;
use frontend::ast::{expr::{
    AssignExpr, BinaryExpr, CallExpr, GroupingExpr, IdentifierExpr, IntLiteralExpr, LogicalExpr,
    FloatLiteralExpr, StrLiteralExpr, UnaryExpr, VisitExpr,
}, stmt::StructStmt};
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

    #[error("can't use '-' token on anything other than an int or a float value")]
    NegateNonNumeric,

    #[error("unknow unary operator '{0}'")]
    UnknownUnaryOp(String),

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

    #[error("wrong arguments number, expected {0} but got {1}")]
    WrongArgsNb(usize, usize),

    #[error("{0}")]
    FnCall(String),

    // Property access
    #[error("only structure instances have properties")]
    NonInstPropAccess,

    #[error("structure has no field '{0}'")]
    InexistantField(EcoString),

    #[error("{0}")]
    InexistantFieldBis(String),

    // Results
    #[error("return")]
    Return(Rc<RefCell<RtVal>>),
}

impl RevReport for InterpErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Interpreter error:".red(), self)
    }
}

pub(crate) type RevResInterp = RevResult<InterpErr>;
pub(crate) type InterpRes = Result<Rc<RefCell<RtVal>>, RevResInterp>;

// --------------
//  Interpreting
// --------------
pub struct Interpreter {
    pub globals: Rc<RefCell<Env>>,
    pub env: Rc<RefCell<Env>>,
    pub locals: HashMap<Loc, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Env::new(None)));


        let _ = globals.borrow_mut().declare_var(
            EcoString::from("clock"),
            Rc::new(RefCell::new(RtVal::NativeFnVal(RevNativeFn {
                name: EcoString::from("clock"),
                arity: 0,
                func: |_, _| {
                    match SystemTime::now().duration_since(UNIX_EPOCH) {
                        Ok(t) => Ok(RtVal::new_float(t.as_millis() as f64 / 1000.).into()),
                        Err(_) => Err(RevResult::new(NativeFnErr::GetTime.into(), None))
                    }
                },
            }))),
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
    pub fn interpret(&mut self, nodes: &Vec<Stmt>, locals: HashMap<Loc, usize>) -> InterpRes {
        self.locals = locals;

        let mut res = RtVal::new_null();

        for node in nodes {
            match node.accept(self) {
                Ok(r) => res = r,
                Err(e) => return Err(e),
            }
        }

        Ok(res)
    }
}

impl VisitStmt<Rc<RefCell<RtVal>>, InterpErr> for Interpreter {
    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> InterpRes {
        stmt.expr.accept(self)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> InterpRes {
        let value = stmt.expr.accept(self)?;
        println!("{}", value.borrow());

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
                RevResult::new(InterpErr::VarDeclEnv(e.to_string()), Some(stmt.loc.clone()))
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
        let tmp = &*cond.borrow();

        match tmp {
            RtVal::BoolVal(b) => match b.value {
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
            _ => Err(RevResult::new(
                InterpErr::NonBoolIfCond,
                Some(stmt.loc.clone()),
            )),
        }
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> InterpRes {
        loop {
            let cond = stmt.condition.accept(self)?;
            let tmp = &*cond.borrow();

            match tmp {
                RtVal::BoolVal(b) => match b.value {
                    true => {
                        stmt.body.accept(self)?;
                    }
                    false => break,
                },
                _ => {
                    return Err(RevResult::new(
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
                .assign(stmt.placeholder.name.clone(), RtVal::new_int(i).into())
                .map_err(|e| {
                    RevResult::new(InterpErr::ForLoop(e.to_string()), Some(stmt.loc.clone()))
                })?;

            stmt.body.accept(self)?;
        }

        let _ = std::mem::replace(&mut self.env, prev_env);

        Ok(RtVal::new_null())
    }

    fn visit_fn_decl_stmt(&mut self, stmt: &FnDeclStmt) -> InterpRes {
        let func = RtVal::new_fn(stmt, self.env.clone());

        self.env
            .borrow_mut()
            .declare_var(stmt.name.clone(), func.into())
            .map_err(|_| {
                RevResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        Ok(RtVal::new_null())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> InterpRes {
        let mut value = RtVal::new_null();

        if let Some(v) = &stmt.value {
            value = v.accept(self)?;
        }

        Err(RevResult::new(InterpErr::Return(value), None))
    }
    
    fn visit_struct_stmt(&mut self, stmt: &StructStmt) -> InterpRes {
        let mut methods: HashMap<EcoString, Function> = HashMap::new();
        stmt.methods.iter().for_each(|m| {
            methods.insert(m.name.clone(), Function::new(m, self.env.clone()));
        });

        // Two steps: declaring and assigning. Allow the struct to reference itself
        // in its methods
        self.env
            .borrow_mut()
            .declare_var(stmt.name.clone(), RtVal::new_null())
            .map_err(|_| {
                RevResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        let mut fields: HashMap<EcoString, Rc<RefCell<RtVal>>> = HashMap::new();
        for f in &stmt.fields {
            let value = match &f.value {
                Some(v) => v.accept(self)?,
                None => RtVal::new_null(),
            };

            fields.insert(f.name.clone(), value);
        }

        let struct_val = RtVal::new_struct(stmt, fields, methods);

        self.env
            .borrow_mut()
            .assign(stmt.name.clone(), Rc::new(RefCell::new(struct_val.clone())))
            .map_err(|_| {
                RevResult::new(
                    InterpErr::VarDeclEnv(stmt.name.to_string()),
                    Some(stmt.loc.clone()),
                )
            })?;

        Ok(RtVal::new_null())
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

impl VisitExpr<Rc<RefCell<RtVal>>, InterpErr> for Interpreter {
    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> InterpRes {
        let lhs = expr.left.accept(self)?;
        if lhs == RtVal::new_null() {
            return Err(RevResult::new(
                InterpErr::UninitializedValue,
                Some(expr.left.get_loc()),
            ));
        }

        let rhs = expr.right.accept(self)?;
        if rhs == RtVal::new_null() {
            return Err(RevResult::new(
                InterpErr::UninitializedValue,
                Some(expr.right.get_loc()),
            ));
        }

        let tmp = rhs.borrow();
        let tmp2 = lhs.borrow();
        match tmp2.operate(&tmp, &expr.operator) {
            Ok(res) => Ok(res.into()),
            Err(e) => Err(RevResult::new(
                InterpErr::OperationEvaluation(e.to_string()),
                Some(expr.right.get_loc()),
            )),
        }
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> InterpRes {
        let value = expr.value.accept(self)?;

        match self.locals.get(&expr.get_loc()) {
            Some(i) => self
                .env
                .borrow_mut()
                .assign_at(expr.name.clone(), value.clone(), i)
                .map_err(|e| {
                    RevResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.value.get_loc()))
                })?,
            None => self.globals.borrow_mut().assign(expr.name.clone(), value.clone()).map_err(|e| {
                RevResult::new(InterpErr::AssignEnv(e.to_string()), Some(expr.value.get_loc()))
            })?,
        }

        Ok(value)
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> InterpRes {
        expr.expr.accept(self)
    }

    fn visit_int_literal_expr(&mut self, expr: &IntLiteralExpr) -> InterpRes {
        Ok(RtVal::new_int(expr.value).into())
    }

    fn visit_float_literal_expr(&mut self, expr: &FloatLiteralExpr) -> InterpRes {
        Ok(RtVal::new_float(expr.value).into())
    }

    fn visit_str_literal_expr(&mut self, expr: &StrLiteralExpr) -> InterpRes {
        Ok(RtVal::new_str(expr.value.clone()).into())
    }

    fn visit_identifier_expr(&mut self, expr: &IdentifierExpr) -> InterpRes {
        match expr.name.as_str() {
            "true" => Ok(RtVal::new_bool(true).into()),
            "false" => Ok(RtVal::new_bool(false).into()),
            "null" => Ok(RtVal::new_null()),
            _ => match self.locals.get(&expr.loc) {
                Some(i) => self
                    .env
                    .borrow()
                    .get_var_at(expr.name.clone(), i)
                    .map_err(|e| {
                        RevResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                    }),
                None => self.globals.borrow().get_var(expr.name.clone()).map_err(|e| {
                    RevResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
                }),
            },
        }
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> InterpRes {
        let value = expr.right.accept(self)?;

        match expr.operator.as_str() {
            "!" => match &mut *value.borrow_mut() {
                RtVal::BoolVal(v) => v.negate(),
                _ => return Err(RevResult::new(
                    InterpErr::BangOpOnNonBool,
                    Some(expr.right.get_loc()),
                ))
            }
            "-" => match &mut *value.borrow_mut() {
                RtVal::IntVal(v) => v.negate(),
                RtVal::FloatVal(v) => v.negate(),
                _ => return Err(RevResult::new(
                    InterpErr::NegateNonNumeric,
                    Some(expr.right.get_loc()),
                ))
            },
            op => return Err(RevResult::new(
                    InterpErr::UnknownUnaryOp(op.into()),
                    Some(expr.right.get_loc()),
                ))
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> InterpRes {
        let left = expr.left.accept(self)?;
        let op = expr.operator.as_str();

        if op == "or" {
            match &*left.clone().borrow() {
                RtVal::BoolVal(b) => {
                    if b.value {
                        return Ok(left);
                    }
                }
                _ => {
                    return Err(RevResult::new(
                        InterpErr::NonBoolIfCond,
                        Some(expr.loc.clone()),
                    ))
                }
            }
        } else if op == "and" {
            match &*left.clone().borrow() {
                RtVal::BoolVal(b) => {
                    if !b.value {
                        return Ok(left);
                    }
                }
                _ => {
                    return Err(RevResult::new(
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

        let mut args: Vec<Rc<RefCell<RtVal>>> = vec![];
        for a in &expr.args {
            args.push(a.accept(self)?);
        }

        let tmp = &*callee.borrow();

        let callable: Box<&dyn Callable> = match tmp {
            RtVal::FuncVal(f) => Box::new(f),
            RtVal::NativeFnVal(f) => Box::new(f),
            RtVal::StructVal(s) => Box::new(s),
            _ => return Err(RevResult::new(InterpErr::NonFnCall, Some(expr.callee.get_loc())))
        };

        if callable.arity() != args.len() {
            return Err(RevResult::new(
                InterpErr::WrongArgsNb(callable.arity(), args.len()),
                Some(expr.loc.clone()),
            ));
        }

        callable.call(self, args).map_err(|e| {
            RevResult::new(InterpErr::FnCall(e.err.to_string()), Some(expr.loc.clone()))
        })
    }
    
    fn visit_get_expr(&mut self, expr: &GetExpr) -> InterpRes {
        let obj = expr.object.accept(self)?;
        let tmp = &*obj.borrow();

        if let RtVal::InstanceVal(inst) = tmp {
            // Field
            if let Some(v) = inst.fields.get(&expr.name) {
                Ok(v.clone())
            // Methods
            } else if let Some(m) = inst.strukt.borrow().methods.get(&expr.name) {
                Ok(m.wrap_bind(obj.clone()))
            } else {
                Err(RevResult::new(InterpErr::InexistantField(expr.name.clone()), Some(expr.loc.clone())))
            }
        } else {
            Err(RevResult::new(InterpErr::NonInstPropAccess, Some(expr.loc.clone())))
        }
    }
    
    fn visit_set_expr(&mut self, expr: &SetExpr) -> InterpRes {
        let obj = expr.object.accept(self)?;
        let mut tmp = obj.borrow_mut();

        match &mut *tmp {
            RtVal::InstanceVal(inst) => {
                let val = expr.value.accept(self)?;

                inst.set(expr.name.clone(), val.clone())
                    .map_err(|e| RevResult::new(InterpErr::InexistantFieldBis(e.to_string()), Some(expr.loc.clone())))?;

                Ok(val)
            },
            _ => Err(RevResult::new(InterpErr::NonInstPropAccess, Some(expr.loc.clone())))
        }
    }
    
    fn visit_self_expr(&mut self, expr: &SelfExpr) -> Result<Rc<RefCell<RtVal>>, RevResult<InterpErr>> {
        self.env.borrow()
            .get_var(expr.name.clone())
            .map_err(|e| {
                    RevResult::new(InterpErr::GetVarEnv(e.to_string()), Some(expr.loc.clone()))
            })
    }
}

#[cfg(test)]
mod tests {
    use ecow::EcoString;

    use crate::{interpreter::InterpErr, utils::lex_parse_resolve_interp};

    #[test]
    fn interp_literals() {
        let code = "1";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "-45.";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-45f64).into()
        );

        let code = "\"hello world!\"";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            EcoString::from("hello world!").into()
        );
    }

    #[test]
    fn interp_binop() {
        let code = "1 +2";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            3.into()
        );

        let code = "1. + -2 *24";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-47f64).into()
        );

        let code = "5 + (6 * (2+3)) - (((6)))";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            29.into()
        );
    }

    #[test]
    fn interp_str_op() {
        let code = "\"foo\" * 4";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "4 * \"foo\"";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            EcoString::from("foofoofoofoo").into()
        );

        let code = "\"foo\" + \" \" + \"bar\"";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            EcoString::from("foo bar").into()
        );

        // Errors
        let code = "\"foo\" * 3.5";
        matches!(
            lex_parse_resolve_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation { .. }
        );

        let code = "\"foo\" + 56";
        matches!(
            lex_parse_resolve_interp(code).err().unwrap().err,
            InterpErr::OperationEvaluation { .. }
        );
    }

    #[test]
    fn interp_negation() {
        let code = "-3";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-3).into()
        );

        let code = "-3.";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-3f64).into()
        );

        let code = "!true";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            false.into()
        );

        let code = "!false";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            true.into()
        );

        // Errors
        let code = "- \"foo\"";
        assert_eq!(
            lex_parse_resolve_interp(code).err().unwrap().err,
            InterpErr::NegateNonNumeric
        );

        let code = "!8";
        assert_eq!(
            lex_parse_resolve_interp(code).err().unwrap().err,
            InterpErr::BangOpOnNonBool
        );
    }

    #[test]
    fn variable() {
        let code = "var a = -8
a";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-8).into()
        );

        let code = "var a = -8
a = 4 + a*2
a";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            (-12).into()
        );

        // Errors
        let code = "a = 5";
        assert!(matches!(
            lex_parse_resolve_interp(code).err().unwrap().err,
            InterpErr::AssignEnv { .. }
        ));

        let code = "var b
3 + b";
        assert_eq!(
            lex_parse_resolve_interp(code).err().unwrap().err,
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
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );
    }

    #[test]
    fn if_stmt() {
        let code = "
var a = true
var b = 0
if a { b = 1 } else {}
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = false
var b = 0
if a { b = 8 } else { b = 1 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = false
var b = 42
if a {} else {}
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            42.into()
        );

        // Errors
        let code = "
var a = 5
if a {} else {}
";
        assert!(
            lex_parse_resolve_interp(code).err().unwrap().err == InterpErr::NonBoolIfCond
        );
    }

    #[test]
    fn logical() {
        let code = "
var a = true
var b = 0
if a and b == 0 { b = 1 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = true
var b = 0
if a and 2 + 2 == 5 { } else { b = 1 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = true
var b = 0
if a or false { b = 1 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = false
var b = 0
if a or false {} else { b = 1 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            1.into()
        );

        let code = "
var a = true
var b = 42
if a and b == 41 or b == 42 and false {} else { b = 45 }
b
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            45.into()
        );
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
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            5.into()
        );
    }

    #[test]
    fn for_stmt() {
        let code = "
var a = 0
for i in 5 { a = a + i }
a
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            10.into()
        );

        let code = "
var a = 0
for i in 5..10 { a = a + i }
a
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            35.into()
        );
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
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            11.into()
        );
    }

    #[test]
    fn first_class_fn() {
        let code = "
fn add(a, b) { return a+b }
var c = add
c(1, 2)
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            3.into()
        );
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
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            6765.into()
        );
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
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            2.into()
        );
    }

    #[test]
    fn block_closure() {
        let code = "
var x = 4
var a 
{
  fn add() {
    return x + 1
  }

  print add()
  var x=2
  a = add()
}

a
";
        assert_eq!(
            *lex_parse_resolve_interp(code).unwrap().borrow(),
            5.into()
        );
    }
}
