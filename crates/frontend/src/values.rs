use std::{cell::RefCell, fmt::Display, rc::Rc};
use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use crate::results::{PhyReport, PhyResult};


#[derive(Debug, Error)]
pub enum RuntimeValErr {
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

    // Others
    #[error("can't use a null value in a binary operation")]
    OperationOnNull,

    #[error("operation not supported")]
    UnknownOperation,
}

impl PhyReport for RuntimeValErr {
    fn get_err_msg(&self) -> String {
        format!("{}", "Runtime value error: ".red())
    }
}

type PhyResRuntimeVal = PhyResult<RuntimeValErr>;

#[derive(Debug, PartialEq, Clone)]
pub enum RuntimeVal {
    IntVal(Rc<RefCell<Int>>),
    RealVal(Rc<RefCell<Real>>),
    StrVal(Rc<RefCell<Str>>),
    BoolVal(Rc<RefCell<Bool>>),
    Null,
}

trait Negate {
    fn negate(&mut self);
}

trait Operate<Rhs> {
    fn operate(&self, rhs: &Rhs, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal>;
}


impl RuntimeVal {
    pub fn new_null() -> Self { RuntimeVal::Null }

    pub fn negate(&self) -> Result<(), PhyResRuntimeVal> {
        match self {
            RuntimeVal::IntVal(i) => i.borrow_mut().negate(),
            RuntimeVal::RealVal(r) => r.borrow_mut().negate(),
            RuntimeVal::BoolVal(b) => b.borrow_mut().negate(),
            _ => {
                return Err(PhyResult::new(RuntimeValErr::UnNegatable, None))
            }
        }

        Ok(())
    }

    // TODO: Error handling for other operation
    pub fn operate(&self, rhs: &RuntimeVal, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match (self, rhs) {
            (RuntimeVal::IntVal(i1), RuntimeVal::IntVal(i2)) => i1.borrow().operate(&*i2.borrow(), operator),
            (RuntimeVal::RealVal(r1), RuntimeVal::RealVal(r2)) => r1.borrow().operate(&*r2.borrow(), operator),
            (RuntimeVal::IntVal(i1), RuntimeVal::RealVal(r1)) => i1.borrow().operate(&*r1.borrow(), operator),
            (RuntimeVal::RealVal(r1), RuntimeVal::IntVal(i1)) => r1.borrow().operate(&*i1.borrow(), operator),
            (RuntimeVal::StrVal(s1), RuntimeVal::StrVal(s2)) => s1.borrow().operate(&*s2.borrow(), operator),
            (RuntimeVal::StrVal(s1), RuntimeVal::IntVal(i1)) => s1.borrow().operate(&*i1.borrow(), operator),
            (RuntimeVal::IntVal(i1), RuntimeVal::StrVal(s1)) => i1.borrow().operate(&*s1.borrow(), operator),
            (RuntimeVal::BoolVal(b1), RuntimeVal::BoolVal(b2)) => b1.borrow().operate(&*b2.borrow(), operator),
            (RuntimeVal::Null, _) | (_, RuntimeVal::Null) => {
                Err(PhyResult::new(RuntimeValErr::OperationOnNull, None))
            }
            _ => Err(PhyResult::new(RuntimeValErr::UnknownOperation, None)),
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
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "+" => Ok((self.value + rhs.value).into()),
            "-" => Ok((self.value - rhs.value).into()),
            "*" => Ok((self.value * rhs.value).into()),
            "/" => Ok((self.value / rhs.value).into()),
            "<" => Ok((self.value < rhs.value).into()),
            ">" => Ok((self.value > rhs.value).into()),
            "<=" => Ok((self.value <= rhs.value).into()),
            ">=" => Ok((self.value >= rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RuntimeValErr::UnsupportedOpOnType(op.to_string(), "int".into()), None)),
        }
    }
}

impl Operate<Real> for Int {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "+" => Ok((self.value as f64 + rhs.value).into()),
            "-" => Ok((self.value as f64 - rhs.value).into()),
            "*" => Ok((self.value as f64 * rhs.value).into()),
            "/" => Ok((self.value as f64 / rhs.value).into()),
            "<" => Ok(((self.value as f64) < rhs.value).into()),
            ">" => Ok((self.value as f64 > rhs.value).into()),
            "<=" => Ok((self.value as f64 <= rhs.value).into()),
            ">=" => Ok((self.value as f64 >= rhs.value).into()),
            "==" => Ok((self.value as f64 == rhs.value).into()),
            "!=" => Ok((self.value as f64 != rhs.value).into()),
            op => Err(PhyResult::new(RuntimeValErr::UnsupportedOpOnType(op.to_string(), "int".into()), None)),
        }
    }
}

impl Operate<Str> for Int {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "*" => Ok(
                EcoString::from(rhs.value.repeat(self.value as usize)).into(),
            ),
            _ => Err(PhyResult::new(RuntimeValErr::OpStrInt, None)),
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
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "+" => Ok((self.value + rhs.value as f64).into()),
            "-" => Ok((self.value - rhs.value as f64).into()),
            "*" => Ok((self.value * rhs.value as f64).into()),
            "/" => Ok((self.value / rhs.value as f64).into()),
            "<" => Ok((self.value < rhs.value as f64).into()),
            ">" => Ok((self.value > rhs.value as f64).into()),
            "<=" => Ok((self.value <= rhs.value as f64).into()),
            ">=" => Ok((self.value >= rhs.value as f64).into()),
            "==" => Ok((self.value == rhs.value as f64).into()),
            "!=" => Ok((self.value != rhs.value as f64).into()),
            op => Err(PhyResult::new(RuntimeValErr::UnsupportedOpOnType(op.to_string(), "real".into()), None)),
        }
    }
}

impl Operate<Real> for Real {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "+" => Ok((self.value + rhs.value).into()),
            "-" => Ok((self.value - rhs.value).into()),
            "*" => Ok((self.value * rhs.value).into()),
            "/" => Ok((self.value / rhs.value).into()),
            "<" => Ok((self.value < rhs.value).into()),
            ">" => Ok((self.value > rhs.value).into()),
            "<=" => Ok((self.value <= rhs.value).into()),
            ">=" => Ok((self.value >= rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RuntimeValErr::UnsupportedOpOnType(op.to_string(), "real".into()), None)),
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
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "+" => Ok(EcoString::from(format!("{}{}", self.value, rhs.value)).into(),),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RuntimeValErr::StringManip(op.to_string()), None)),
        }
    }
}

impl Operate<Int> for Str {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "*" => Ok(
                EcoString::from(self.value.repeat(rhs.value as usize)).into(),
            ),
            _ => Err(PhyResult::new(RuntimeValErr::OpStrInt, None)),
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
    fn operate(&self, rhs: &Bool, operator: &str) -> Result<RuntimeVal, PhyResRuntimeVal> {
        match operator {
            "and" => Ok((self.value && rhs.value).into()),
            "or" => Ok((self.value || rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RuntimeValErr::UnsupportedOpOnType(op.to_string(), "bool".into()), None)),
        }
    }
}


// --------
//   Into
// --------
impl From<i64> for RuntimeVal {
    fn from(value: i64) -> Self {
        RuntimeVal::IntVal(Rc::new(RefCell::new(Int { value })))
    }
}

impl From<f64> for RuntimeVal {
    fn from(value: f64) -> Self {
        RuntimeVal::RealVal(Rc::new(RefCell::new(Real { value })))
    }
}

impl From<EcoString> for RuntimeVal {
    fn from(value: EcoString) -> Self {
        RuntimeVal::StrVal(Rc::new(RefCell::new(Str {
            value: value.clone(),
        })))
    }
}

impl From<String> for RuntimeVal {
    fn from(value: String) -> Self {
        RuntimeVal::StrVal(Rc::new(RefCell::new(Str {
            value: value.into(),
        })))
    }
}

impl From<bool> for RuntimeVal {
    fn from(value: bool) -> Self {
        RuntimeVal::BoolVal(Rc::new(RefCell::new(Bool { value })))
    }
}


// -----------
//   Display
// -----------
impl Display for RuntimeVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeVal::IntVal(i) => write!(f, "{}", i.borrow().value),
            RuntimeVal::RealVal(r) => write!(f, "{}", r.borrow().value),
            RuntimeVal::BoolVal(b) => write!(f, "{}", b.borrow().value),
            RuntimeVal::StrVal(s) => write!(f, "\"{}\"", s.borrow().value),
            RuntimeVal::Null => write!(f, "null"),
        }
    }
}
