use std::rc::Rc;

use ecow::EcoString;

pub enum Values {
    IntVal(Rc<Int>),
    RealVal(Rc<Real>),
    StrVal(Rc<Str>),
    BoolVal(Rc<Bool>),
}

pub struct Int {
    value: i64,
}
pub struct Real {
    value: f64,
}
pub struct Str {
    value: EcoString,
}
pub struct Bool {
    value: bool,
}

impl From<i64> for Values {
    fn from(value: i64) -> Self {
        Values::IntVal(Rc::new(Int { value }))
    }
}

impl From<f64> for Values {
    fn from(value: f64) -> Self {
        Values::RealVal(Rc::new(Real { value }))
    }
}

impl From<&EcoString> for Values {
    fn from(value: &EcoString) -> Self {
        Values::StrVal(Rc::new(Str {
            value: value.clone(),
        }))
    }
}

