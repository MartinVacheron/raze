use std::{cell::RefCell, fmt::Display, rc::Rc};
use ecow::EcoString;
use crate::results::PhyResult;

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
    fn operate(&self, rhs: &Rhs, operator: &str) -> Result<RuntimeVal, PhyResult>;
}


impl RuntimeVal {
    pub fn new_null() -> Self { RuntimeVal::Null }

    pub fn negate(&self) -> Result<(), PhyResult> {
        match self {
            RuntimeVal::IntVal(i) => i.borrow_mut().negate(),
            RuntimeVal::RealVal(r) => r.borrow_mut().negate(),
            RuntimeVal::BoolVal(b) => b.borrow_mut().negate(),
            _ => {
                return Err(PhyResult::value_error(
                    "Can't negate a value that isn't either: int, real or bool".into(),
                ))
            }
        }

        Ok(())
    }

    // TODO: Error handling for other operation
    pub fn operate(&self, rhs: &RuntimeVal, operator: &str) -> Result<RuntimeVal, PhyResult> {
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
                Err(PhyResult::value_error("Can't use a null value in a binary operation".into()))
            }
            _ => Err(PhyResult::value_error("Operation not supported".into())),
        }
    }
}

// -------
//   Int
// -------
pub struct Int {
    pub value: i64,
}

impl Negate for Int {
    fn negate(&mut self) {
        self.value *= -1;
    }
}

impl Operate<Int> for Int {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResult> {
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
            op => Err(PhyResult::value_error(
                format!(
                    "Operator '{}' is not supported for operations on int type",
                    op
                )
            )),
        }
    }
}

impl Operate<Real> for Int {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RuntimeVal, PhyResult> {
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
            op => Err(PhyResult::value_error(
                format!(
                    "Operator '{}' is not supported for operations on int type",
                    op
                )
            )),
        }
    }
}

impl Operate<Str> for Int {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RuntimeVal, PhyResult> {
        match operator {
            "*" => Ok(
                EcoString::from(rhs.value.repeat(self.value as usize)).into(),
            ),
            _ => Err(PhyResult::value_error(
                format!("Can't use this operator for operations on string and int types").into(),
            )),
        }
    }
}


// --------
//   Real
// --------
pub struct Real {
    pub value: f64,
}

impl Negate for Real {
    fn negate(&mut self) {
        self.value *= -1.;
    }
}

impl Operate<Int> for Real {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResult> {
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
            op => Err(PhyResult::value_error(
                format!(
                    "Operator '{}' is not supported for operations on real type",
                    op
                )
            )),
        }
    }
}

impl Operate<Real> for Real {
    fn operate(&self, rhs: &Real, operator: &str) -> Result<RuntimeVal, PhyResult> {
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
            op => Err(PhyResult::value_error(
                format!(
                    "Operator '{}' is not supported for operations on int type",
                    op
                )
            )),
        }
    }
}

// ----------
//   String
// ----------
pub struct Str {
    pub value: EcoString,
}

impl Operate<Str> for Str {
    fn operate(&self, rhs: &Str, operator: &str) -> Result<RuntimeVal, PhyResult> {
        match operator {
            "+" => Ok(EcoString::from(format!("{}{}", self.value, rhs.value)).into(),),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::value_error(
                format!("Operator '{}' is not supported for string manipulation", op).into(),
            )),
        }
    }
}

impl Operate<Int> for Str {
    fn operate(&self, rhs: &Int, operator: &str) -> Result<RuntimeVal, PhyResult> {
        match operator {
            "*" => Ok(
                EcoString::from(self.value.repeat(rhs.value as usize)).into(),
            ),
            _ => Err(PhyResult::value_error(
                format!("Can't use this operator for operations on string and int types").into(),
            )),
        }
    }
}


// --------
//   Bool
// --------
pub struct Bool {
    pub value: bool,
}

impl Negate for Bool {
    fn negate(&mut self) {
        self.value = !self.value;
    }
}

impl Operate<Bool> for Bool {
    fn operate(&self, rhs: &Bool, operator: &str) -> Result<RuntimeVal, PhyResult> {
        match operator {
            "and" => Ok((self.value && rhs.value).into()),
            "or" => Ok((self.value || rhs.value).into()),
            "==" => Ok((self.value == rhs.value).into()),
            "!=" => Ok((self.value != rhs.value).into()),
            op => Err(PhyResult::value_error(format!(
                "Operator '{}' is not supported for operations on bool type",
                op
            ))),
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
