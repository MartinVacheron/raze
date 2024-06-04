use std::{cell::RefCell, rc::Rc};

use ecow::EcoString;

use crate::results::PhyResult;

pub enum Values {
    IntVal(Rc<RefCell<Int>>),
    RealVal(Rc<RefCell<Real>>),
    StrVal(Rc<RefCell<Str>>),
    BoolVal(Rc<RefCell<Bool>>),
    Null,
}

impl Values {
    pub fn negate(&self) -> Result<(), PhyResult> {
        match self {
            Values::IntVal(i) => i.borrow_mut().value *= -1,
            Values::RealVal(r) => r.borrow_mut().value *= -1.,
            Values::BoolVal(b) => b.borrow_mut().value = !b.borrow().value,
            _ => {
                return Err(PhyResult::value_error(
                    "Can't negate a value that isn't either: int, real or bool".into(),
                ))
            }
        }

        Ok(())
    }

    // TODO: Error handling for other operation
    pub fn operate(&self, rhs: &Values, operator: &str) -> Result<Values, PhyResult> {
        match (self, rhs) {
            (Values::IntVal(i1), Values::IntVal(i2)) => match operator {
                "+" => Ok((i1.borrow().value + i2.borrow().value).into()),
                "-" => Ok((i1.borrow().value - i2.borrow().value).into()),
                "*" => Ok((i1.borrow().value * i2.borrow().value).into()),
                "/" => Ok((i1.borrow().value / i2.borrow().value).into()),
                "<" => Ok((i1.borrow().value < i2.borrow().value).into()),
                ">" => Ok((i1.borrow().value > i2.borrow().value).into()),
                "<=" => Ok((i1.borrow().value <= i2.borrow().value).into()),
                ">=" => Ok((i1.borrow().value >= i2.borrow().value).into()),
                "==" => Ok((i1.borrow().value == i2.borrow().value).into()),
                "!=" => Ok((i1.borrow().value != i2.borrow().value).into()),
                op => Err(PhyResult::value_error(
                    format!(
                        "Operator '{}' is not supported for operations on int type",
                        op
                    )
                    .into(),
                )),
            },
            (Values::RealVal(r1), Values::RealVal(r2)) => match operator {
                "+" => Ok((r1.borrow().value + r2.borrow().value).into()),
                "-" => Ok((r1.borrow().value - r2.borrow().value).into()),
                "*" => Ok((r1.borrow().value * r2.borrow().value).into()),
                "/" => Ok((r1.borrow().value / r2.borrow().value).into()),
                "<" => Ok((r1.borrow().value < r2.borrow().value).into()),
                ">" => Ok((r1.borrow().value > r2.borrow().value).into()),
                "<=" => Ok((r1.borrow().value <= r2.borrow().value).into()),
                ">=" => Ok((r1.borrow().value >= r2.borrow().value).into()),
                "==" => Ok((r1.borrow().value == r2.borrow().value).into()),
                "!=" => Ok((r1.borrow().value != r2.borrow().value).into()),
                op => Err(PhyResult::value_error(
                    format!(
                        "Operator '{}' is not supported for operations on real type",
                        op
                    )
                    .into(),
                )),
            },
            (Values::IntVal(i1), Values::RealVal(r1)) => match operator {
                "+" => Ok((i1.borrow().value as f64 + r1.borrow().value).into()),
                "-" => Ok((i1.borrow().value as f64 - r1.borrow().value).into()),
                "*" => Ok((i1.borrow().value as f64 * r1.borrow().value).into()),
                "/" => Ok((i1.borrow().value as f64 / r1.borrow().value).into()),
                "<" => Ok(((i1.borrow().value as f64) < r1.borrow().value).into()),
                ">" => Ok((i1.borrow().value as f64 > r1.borrow().value).into()),
                "<=" => Ok((i1.borrow().value as f64 <= r1.borrow().value).into()),
                ">=" => Ok((i1.borrow().value as f64 >= r1.borrow().value).into()),
                "==" => Ok((i1.borrow().value as f64 == r1.borrow().value).into()),
                "!=" => Ok((i1.borrow().value as f64 != r1.borrow().value).into()),
                op => Err(PhyResult::value_error(
                    format!(
                        "Operator '{}' is not supported for operations on real type",
                        op
                    )
                    .into(),
                )),
            },
            (Values::RealVal(r1), Values::IntVal(i1)) => match operator {
                "+" => Ok((r1.borrow().value + i1.borrow().value as f64).into()),
                "-" => Ok((r1.borrow().value - i1.borrow().value as f64).into()),
                "*" => Ok((r1.borrow().value * i1.borrow().value as f64).into()),
                "/" => Ok((r1.borrow().value / i1.borrow().value as f64).into()),
                "<" => Ok((r1.borrow().value < i1.borrow().value as f64).into()),
                ">" => Ok((r1.borrow().value > i1.borrow().value as f64).into()),
                "<=" => Ok((r1.borrow().value <= i1.borrow().value as f64).into()),
                ">=" => Ok((r1.borrow().value >= i1.borrow().value as f64).into()),
                "==" => Ok((r1.borrow().value == i1.borrow().value as f64).into()),
                "!=" => Ok((r1.borrow().value != i1.borrow().value as f64).into()),
                op => Err(PhyResult::value_error(
                    format!(
                        "Operator '{}' is not supported for operations on real type",
                        op
                    )
                    .into(),
                )),
            },
            (Values::StrVal(s1), Values::StrVal(s2)) => match operator {
                "+" => Ok(
                    EcoString::from(format!("{}{}", s1.borrow().value, s2.borrow().value)).into(),
                ),
                "==" => Ok((s1.borrow().value == s2.borrow().value).into()),
                "!=" => Ok((s1.borrow().value != s2.borrow().value).into()),
                op => Err(PhyResult::value_error(
                    format!("Operator '{}' is not supported for string manipulation", op).into(),
                )),
            },
            (Values::StrVal(s1), Values::IntVal(i1)) => match operator {
                "*" => Ok(
                    EcoString::from(s1.borrow().value.repeat(i1.borrow().value as usize)).into(),
                ),
                op => Err(PhyResult::value_error(
                    format!("Operator '{}' is not supported for string repetition", op).into(),
                )),
            },
            (Values::IntVal(i1), Values::StrVal(s1)) => match operator {
                "*" => Ok(
                    EcoString::from(s1.borrow().value.repeat(i1.borrow().value as usize)).into(),
                ),
                op => Err(PhyResult::value_error(
                    format!("Operator '{}' is not supported for string repetition", op).into(),
                )),
            },
            (Values::BoolVal(b1), Values::BoolVal(b2)) => match operator {
                "and" => Ok((b1.borrow().value && b2.borrow().value).into()),
                "or" => Ok((b1.borrow().value || b2.borrow().value).into()),
                "==" => Ok((b1.borrow().value == b2.borrow().value).into()),
                "!=" => Ok((b1.borrow().value != b2.borrow().value).into()),
                op => Err(PhyResult::value_error(format!(
                    "Operator '{}' is not supported for operations on bool type",
                    op
                ))),
            },
            (Values::Null, _) | (_, Values::Null) => {
                Err(PhyResult::value_error("Can't use a null value in a binary operation".into()))
            }
            _ => Err(PhyResult::value_error("Operation not supported".into())),
        }
    }
}

pub struct Int {
    pub value: i64,
}
pub struct Real {
    pub value: f64,
}
pub struct Str {
    pub value: EcoString,
}
pub struct Bool {
    pub value: bool,
}

impl From<i64> for Values {
    fn from(value: i64) -> Self {
        Values::IntVal(Rc::new(RefCell::new(Int { value })))
    }
}

impl From<f64> for Values {
    fn from(value: f64) -> Self {
        Values::RealVal(Rc::new(RefCell::new(Real { value })))
    }
}

impl From<EcoString> for Values {
    fn from(value: EcoString) -> Self {
        Values::StrVal(Rc::new(RefCell::new(Str {
            value: value.clone(),
        })))
    }
}

impl From<bool> for Values {
    fn from(value: bool) -> Self {
        Values::BoolVal(Rc::new(RefCell::new(Bool { value })))
    }
}
