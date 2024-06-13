use std::{cell::RefCell, fmt::Display, rc::Rc};
use colored::Colorize;
use ecow::EcoString;
use thiserror::Error;
use crate::results::{PhyReport, PhyResult};


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

    // Others
    #[error("can't use a null value in a binary operation")]
    OperationOnNull,

    #[error("operation not supported")]
    UnknownOperation,
}

impl PhyReport for RtValErr {
    fn get_err_msg(&self) -> String {
        format!("{}", "Runtime value error: ".red())
    }
}

type PhyResRtVal = PhyResult<RtValErr>;


// ----------------
//  Runtime Values
// ----------------
#[derive(Debug, PartialEq, Clone)]
pub enum RtValKind {
    IntVal(Rc<RefCell<Int>>),
    RealVal(Rc<RefCell<Real>>),
    StrVal(Rc<RefCell<Str>>),
    BoolVal(Rc<RefCell<Bool>>),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
pub enum RtType {
    None,
    Defined,
    Infered,
    Any,
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
    fn operate(&self, rhs: &Rhs, operator: &str) -> Result<RtVal, PhyResRtVal>;
}


impl RtVal {
    pub fn new_null() -> Self {
        Self {
            value: RtValKind::Null,
            typ: RtType::Defined,
        }
    }

    pub fn negate(&self) -> Result<(), PhyResRtVal> {
        match &self.value {
            RtValKind::IntVal(i) => i.borrow_mut().negate(),
            RtValKind::RealVal(r) => r.borrow_mut().negate(),
            RtValKind::BoolVal(b) => b.borrow_mut().negate(),
            _ => {
                return Err(PhyResult::new(RtValErr::UnNegatable, None))
            }
        }

        Ok(())
    }

    // TODO: Error handling for other operation
    pub fn operate(&self, rhs: &RtVal, operator: &str) -> Result<RtVal, PhyResRtVal> {
        match (&self.value, &rhs.value) {
            (RtValKind::IntVal(i1), RtValKind::IntVal(i2)) => i1.borrow().operate(&*i2.borrow(), operator),
            (RtValKind::RealVal(r1), RtValKind::RealVal(r2)) => r1.borrow().operate(&*r2.borrow(), operator),
            (RtValKind::IntVal(i1), RtValKind::RealVal(r1)) => i1.borrow().operate(&*r1.borrow(), operator),
            (RtValKind::RealVal(r1), RtValKind::IntVal(i1)) => r1.borrow().operate(&*i1.borrow(), operator),
            (RtValKind::StrVal(s1), RtValKind::StrVal(s2)) => s1.borrow().operate(&*s2.borrow(), operator),
            (RtValKind::StrVal(s1), RtValKind::IntVal(i1)) => s1.borrow().operate(&*i1.borrow(), operator),
            (RtValKind::IntVal(i1), RtValKind::StrVal(s1)) => i1.borrow().operate(&*s1.borrow(), operator),
            (RtValKind::BoolVal(b1), RtValKind::BoolVal(b2)) => b1.borrow().operate(&*b2.borrow(), operator),
            (RtValKind::Null, _) | (_, RtValKind::Null) => {
                Err(PhyResult::new(RtValErr::OperationOnNull, None))
            }
            _ => Err(PhyResult::new(RtValErr::UnknownOperation, None)),
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
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, PhyResRtVal> {
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
            op => Err(PhyResult::new(RtValErr::UnsupportedOpOnType(op.to_string(), "int".into()), None)),
        }
    }
}

impl Operate<Real> for Int {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RtVal, PhyResRtVal> {
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
            op => Err(PhyResult::new(RtValErr::UnsupportedOpOnType(op.to_string(), "int".into()), None)),
        }
    }
}

impl Operate<Str> for Int {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RtVal, PhyResRtVal> {
        match operator {
            "*" => Ok(
                EcoString::from(rhs.value.repeat(self.value as usize)).into(),
            ),
            _ => Err(PhyResult::new(RtValErr::OpStrInt, None)),
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
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, PhyResRtVal> {
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
            op => Err(PhyResult::new(RtValErr::UnsupportedOpOnType(op.to_string(), "real".into()), None)),
        }
    }
}

impl Operate<Real> for Real {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RtVal, PhyResRtVal> {
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
            op => Err(PhyResult::new(RtValErr::UnsupportedOpOnType(op.to_string(), "real".into()), None)),
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
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RtVal, PhyResRtVal> {
        match operator {
            "+" => Ok(EcoString::from(format!("{}{}", self.value, rhs.value)).into(),),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RtValErr::StringManip(op.to_string()), None)),
        }
    }
}

impl Operate<Int> for Str {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RtVal, PhyResRtVal> {
        match operator {
            "*" => Ok(
                EcoString::from(self.value.repeat(rhs.value as usize)).into(),
            ),
            _ => Err(PhyResult::new(RtValErr::OpStrInt, None)),
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
    fn operate(&self, rhs: &Bool, operator: &str) -> Result<RtVal, PhyResRtVal> {
        match operator {
            "and" => Ok((self.value && rhs.value).into()),
            "or" => Ok((self.value || rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::new(RtValErr::UnsupportedOpOnType(op.to_string(), "bool".into()), None)),
        }
    }
}


// --------
//   Into
// --------
impl From<i64> for RtVal {
    fn from(value: i64) -> Self {
        Self {
            value: RtValKind::IntVal(Rc::new(RefCell::new(Int { value }))),
            typ: RtType::Defined,
        }
    }
}

impl From<f64> for RtVal {
    fn from(value: f64) -> Self {
        Self {
            value: RtValKind::RealVal(Rc::new(RefCell::new(Real { value }))),
            typ: RtType::Defined,
        }
    }
}

impl From<EcoString> for RtVal {
    fn from(value: EcoString) -> Self {
        Self {
            value: RtValKind::StrVal(Rc::new(RefCell::new(Str { value: value.clone() }))),
            typ: RtType::Defined,
        }
    }
}

impl From<String> for RtVal {
    fn from(value: String) -> Self {
        Self {
            value: RtValKind::StrVal(Rc::new(RefCell::new(Str { value: value.into() }))),
            typ: RtType::Defined,
        }
    }
}

impl From<bool> for RtVal {
    fn from(value: bool) -> Self {
        Self {
            value: RtValKind::BoolVal(Rc::new(RefCell::new(Bool { value }))),
            typ: RtType::Defined,

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
            RtValKind::Null => write!(f, "null"),
        }
    }
}
