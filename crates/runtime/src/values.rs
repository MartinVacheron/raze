use colored::*;
use ecow::EcoString;
use std::{cell::RefCell, fmt::Display, rc::Rc};
use thiserror::Error;
use frontend::ast::stmt::{FnDeclStmt, Stmt};
use tools::results::{PhyReport, PhyResult};

use crate::{
    callable::Callable, environment::Env, interpreter::{InterpErr, Interpreter}, native_functions::PhyNativeFn
};


// -----------------
//  Error managment
// -----------------
#[derive(Debug, Error)]
pub enum RtValErr {
    // Negation
    #[error("can't negate a value that isn't either of type: int, real or bool")]
    UnNegatable,

    // Types operations
    #[error("operator '{0}' is not supported for operations on {1} type")]
    UnsupportedOpOnType(String, String),

    #[error("can't use this operator for operations on string and int types")]
    OpStrInt,

    #[error("operator '{0}' is not supported for string manipulation")]
    StringManip(String),

    // Function
    #[error("function parameter declaration")]
    WrongFnParamDecl,

    #[error("{0}")]
    FnExecution(String),

    // Others
    #[error("can't use a null value in a binary operation")]
    OperationOnNull,

    #[error("operation not supported")]
    UnknownOperation,
}

impl PhyReport for RtValErr {
    fn get_err_msg(&self) -> String {
        format!("{}: {}", "Function error".red(), self)
    }
}

// ----------------
//  Runtime Values
// ----------------
#[derive(Debug, PartialEq, Clone)]
pub enum RtValKind {
    IntVal(Rc<RefCell<Int>>),
    RealVal(Rc<RefCell<Real>>),
    StrVal(Rc<RefCell<Str>>),
    BoolVal(Rc<RefCell<Bool>>),
    FuncVal(Rc<Function>),
    NativeFnVal(Rc<PhyNativeFn>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RtType {
    None,
    Defined(DefinedType),
    Infered,
    Any,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DefinedType {
    Null,
    Int,
    Real,
    String,
    Bool,
    Fn(EcoString),
    NativeFn,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RtVal {
    pub value: RtValKind,
    pub typ: RtType,
}

trait Negate {
    fn negate(&mut self);
}

trait Operate<Rhs> {
    fn operate(&self, rhs: &Rhs, operator: &str) -> Result<RtVal, RtValErr>;
}

impl RtVal {
    pub fn new_null() -> Self {
        Self {
            value: RtValKind::Null,
            typ: RtType::Defined(DefinedType::Null),
        }
    }

    pub fn negate(&self) -> Result<(), RtValErr> {
        match &self.value {
            RtValKind::IntVal(i) => i.borrow_mut().negate(),
            RtValKind::RealVal(r) => r.borrow_mut().negate(),
            RtValKind::BoolVal(b) => b.borrow_mut().negate(),
            _ => return Err(RtValErr::UnNegatable),
        }

        Ok(())
    }

    // TODO: Error handling for other operation
    pub fn operate(&self, rhs: &RtVal, operator: &str) -> Result<RtVal, RtValErr> {
        match (&self.value, &rhs.value) {
            (RtValKind::IntVal(i1), RtValKind::IntVal(i2)) => {
                i1.borrow().operate(&*i2.borrow(), operator)
            }
            (RtValKind::RealVal(r1), RtValKind::RealVal(r2)) => {
                r1.borrow().operate(&*r2.borrow(), operator)
            }
            (RtValKind::IntVal(i1), RtValKind::RealVal(r1)) => {
                i1.borrow().operate(&*r1.borrow(), operator)
            }
            (RtValKind::RealVal(r1), RtValKind::IntVal(i1)) => {
                r1.borrow().operate(&*i1.borrow(), operator)
            }
            (RtValKind::StrVal(s1), RtValKind::StrVal(s2)) => {
                s1.borrow().operate(&*s2.borrow(), operator)
            }
            (RtValKind::StrVal(s1), RtValKind::IntVal(i1)) => {
                s1.borrow().operate(&*i1.borrow(), operator)
            }
            (RtValKind::IntVal(i1), RtValKind::StrVal(s1)) => {
                i1.borrow().operate(&*s1.borrow(), operator)
            }
            (RtValKind::BoolVal(b1), RtValKind::BoolVal(b2)) => {
                b1.borrow().operate(&*b2.borrow(), operator)
            }
            (RtValKind::Null, _) | (_, RtValKind::Null) => Err(RtValErr::OperationOnNull),
            _ => Err(RtValErr::UnknownOperation),
        }
    }
}

// -------
//   Int
// -------
#[derive(Debug, PartialEq)]
pub struct Int {
    pub value: i64,
}

impl Negate for Int {
    fn negate(&mut self) {
        self.value *= -1;
    }
}

impl Operate<Int> for Int {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "+" => Ok((self.value + rhs.value).into()),
            "-" => Ok((self.value - rhs.value).into()),
            "*" => Ok((self.value * rhs.value).into()),
            "/" => Ok((self.value / rhs.value).into()),
            "%" => Ok((self.value % rhs.value).into()),
            "<" => Ok((self.value < rhs.value).into()),
            ">" => Ok((self.value > rhs.value).into()),
            "<=" => Ok((self.value <= rhs.value).into()),
            ">=" => Ok((self.value >= rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(RtValErr::UnsupportedOpOnType(op.to_string(), "int".into())),
        }
    }
}

impl Operate<Real> for Int {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "+" => Ok((self.value as f64 + rhs.value).into()),
            "-" => Ok((self.value as f64 - rhs.value).into()),
            "*" => Ok((self.value as f64 * rhs.value).into()),
            "/" => Ok((self.value as f64 / rhs.value).into()),
            "%" => Ok((self.value as f64 % rhs.value).into()),
            "<" => Ok(((self.value as f64) < rhs.value).into()),
            ">" => Ok((self.value as f64 > rhs.value).into()),
            "<=" => Ok((self.value as f64 <= rhs.value).into()),
            ">=" => Ok((self.value as f64 >= rhs.value).into()),
            "==" => Ok((self.value as f64 == rhs.value).into()),
            "!=" => Ok((self.value as f64 != rhs.value).into()),
            op => Err(RtValErr::UnsupportedOpOnType(op.to_string(), "int".into())),
        }
    }
}

impl Operate<Str> for Int {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "*" => Ok(rhs.value.repeat(self.value as usize).into()),
            _ => Err(RtValErr::OpStrInt),
        }
    }
}

// --------
//   Real
// --------
#[derive(Debug, PartialEq)]
pub struct Real {
    pub value: f64,
}

impl Negate for Real {
    fn negate(&mut self) {
        self.value *= -1.;
    }
}

impl Operate<Int> for Real {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "+" => Ok((self.value + rhs.value as f64).into()),
            "-" => Ok((self.value - rhs.value as f64).into()),
            "*" => Ok((self.value * rhs.value as f64).into()),
            "/" => Ok((self.value / rhs.value as f64).into()),
            "%" => Ok((self.value % rhs.value as f64).into()),
            "<" => Ok((self.value < rhs.value as f64).into()),
            ">" => Ok((self.value > rhs.value as f64).into()),
            "<=" => Ok((self.value <= rhs.value as f64).into()),
            ">=" => Ok((self.value >= rhs.value as f64).into()),
            "==" => Ok((self.value == rhs.value as f64).into()),
            "!=" => Ok((self.value != rhs.value as f64).into()),
            op => Err(RtValErr::UnsupportedOpOnType(op.to_string(), "real".into())),
        }
    }
}

impl Operate<Real> for Real {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "+" => Ok((self.value + rhs.value).into()),
            "-" => Ok((self.value - rhs.value).into()),
            "*" => Ok((self.value * rhs.value).into()),
            "/" => Ok((self.value / rhs.value).into()),
            "%" => Ok((self.value % rhs.value).into()),
            "<" => Ok((self.value < rhs.value).into()),
            ">" => Ok((self.value > rhs.value).into()),
            "<=" => Ok((self.value <= rhs.value).into()),
            ">=" => Ok((self.value >= rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(RtValErr::UnsupportedOpOnType(op.to_string(), "real".into())),
        }
    }
}

// ----------
//   String
// ----------
#[derive(Debug, PartialEq)]
pub struct Str {
    pub value: EcoString,
}

impl Operate<Str> for Str {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "+" => Ok(EcoString::from(format!("{}{}", self.value, rhs.value)).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(RtValErr::StringManip(op.to_string())),
        }
    }
}

impl Operate<Int> for Str {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "*" => Ok(self.value.repeat(rhs.value as usize).into()),
            _ => Err(RtValErr::OpStrInt),
        }
    }
}

// --------
//   Bool
// --------
#[derive(Debug, PartialEq)]
pub struct Bool {
    pub value: bool,
}

impl Negate for Bool {
    fn negate(&mut self) {
        self.value = !self.value;
    }
}

impl Operate<Bool> for Bool {
    fn operate(&self, rhs: &Bool, operator: &str) -> Result<RtVal, RtValErr> {
        match operator {
            "and" => Ok((self.value && rhs.value).into()),
            "or" => Ok((self.value || rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(RtValErr::UnsupportedOpOnType(op.to_string(), "bool".into())),
        }
    }
}

// ------------
//   Function
// ------------
#[derive(Debug)]
pub struct Function {
    pub name: EcoString,
    pub params: Rc<Vec<EcoString>>,
    pub body: Rc<Vec<Stmt>>,
    pub closure: Rc<RefCell<Env>>,
}

impl RtVal {
    pub fn new_fn(value: &FnDeclStmt, closure: Rc<RefCell<Env>>) -> Self {
        Self {
            value: RtValKind::FuncVal(Rc::new(Function {
                name: value.name.clone(),
                params: value.params.clone(),
                body: value.body.clone(),
                closure: closure.clone(),
            })),
            typ: RtType::Any,
        }
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Callable<RtValErr> for Function {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: Vec<RtVal>,
    ) -> Result<RtVal, PhyResult<RtValErr>> {
        let mut new_env = Env::new(Some(self.closure.clone()));

        for (p, v) in self.params.iter().zip(args) {
            new_env
                .declare_var(p.clone(), v)
                .map_err(|_| PhyResult::new(RtValErr::WrongFnParamDecl, None))?;
        }

        match interpreter.execute_block_stmt(&self.body, new_env) {
            Ok(_) => Ok(RtVal::new_null()),
            Err(e) => match e.err {
                InterpErr::Return(v) => Ok(v),
                _ => Err(PhyResult::new(RtValErr::FnExecution(e.err.to_string()), None))
            }
        }
    }

    fn arity(&self) -> usize {
        self.params.len()
    }
}


// --------
//   Into
// --------
impl From<i64> for RtVal {
    fn from(value: i64) -> Self {
        Self {
            value: RtValKind::IntVal(Rc::new(RefCell::new(Int { value }))),
            typ: RtType::Defined(DefinedType::Int),
        }
    }
}

impl From<f64> for RtVal {
    fn from(value: f64) -> Self {
        Self {
            value: RtValKind::RealVal(Rc::new(RefCell::new(Real { value }))),
            typ: RtType::Defined(DefinedType::Real),
        }
    }
}

impl From<EcoString> for RtVal {
    fn from(value: EcoString) -> Self {
        Self {
            value: RtValKind::StrVal(Rc::new(RefCell::new(Str {
                value: value.clone(),
            }))),
            typ: RtType::Defined(DefinedType::String),
        }
    }
}

impl From<String> for RtVal {
    fn from(value: String) -> Self {
        Self {
            value: RtValKind::StrVal(Rc::new(RefCell::new(Str {
                value: value.into(),
            }))),
            typ: RtType::Defined(DefinedType::String),
        }
    }
}

impl From<bool> for RtVal {
    fn from(value: bool) -> Self {
        Self {
            value: RtValKind::BoolVal(Rc::new(RefCell::new(Bool { value }))),
            typ: RtType::Defined(DefinedType::Bool),
        }
    }
}


// -----------
//   Display
// -----------
impl Display for RtVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.value {
            RtValKind::IntVal(i) => write!(f, "{}", i.borrow().value),
            RtValKind::RealVal(r) => write!(f, "{}", r.borrow().value),
            RtValKind::BoolVal(b) => write!(f, "{}", b.borrow().value),
            RtValKind::StrVal(s) => write!(f, "\"{}\"", s.borrow().value),
            RtValKind::FuncVal(func) => write!(f, "<fn {}>", func.name),
            RtValKind::NativeFnVal(func) => write!(f, "{}", func),
            RtValKind::Null => write!(f, "null"),
        }
    }
}
