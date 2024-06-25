use colored::*;
use ecow::EcoString;
use std::{fmt, rc::Rc, time::{SystemTime, UNIX_EPOCH}};
use thiserror::Error;

use crate::{
    callable::Callable,
    interpreter::Interpreter,
    values::RtVal,
};
use tools::results::{PhyReport, PhyResult};


pub type NativeFnRes = Result<RtVal, PhyResult<NativeFnErr>>;

pub struct PhyNativeFn {
    pub name: EcoString,
    pub func: Rc<dyn Callable<NativeFnErr>>,
}

impl fmt::Debug for PhyNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}

impl PartialEq for PhyNativeFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name 
    }
}

impl fmt::Display for PhyNativeFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<native fn>")
    }
}


#[derive(Debug, Error)]
pub enum NativeFnErr {
    #[error("time access failed")]
    GetTime,
}

impl PhyReport for NativeFnErr {
    fn get_err_msg(&self) -> String {
        format!("{} {}", "Function error:".red(), self)
    }
}


// Clock
pub struct NativeClock;

impl Callable<NativeFnErr> for NativeClock {
    fn arity(&self) -> usize {
        0
    }

    fn call(&self, _: &Interpreter, _: Vec<RtVal>) -> NativeFnRes {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(t) => Ok((t.as_millis() as f64 / 1000.).into()),
            Err(_) => Err(PhyResult::new(NativeFnErr::GetTime, None))
        }
    }
}
