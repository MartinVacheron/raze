use colored::*;
use std::time::{SystemTime, UNIX_EPOCH};
use thiserror::Error;

use crate::{
    callable::Callable,
    interpreter::Interpreter,
    results::{PhyReport, PhyResult},
    values::RtVal,
};


pub type NativeFnRes = Result<RtVal, PhyResult<NativeFnErr>>;

pub struct Clock {}

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

impl Callable<NativeFnErr> for Clock {
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
