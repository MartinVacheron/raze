use std::{cell::RefCell, rc::Rc};

use colored::Colorize;
use thiserror::Error;
use rizon_tools::results::{RizonReport, RizonResult};

use crate::{interpreter::Interpreter, native_functions::NativeFnErr, values::RtVal};


pub type CallRes = Result<Rc<RefCell<RtVal>>, RizonResult<CallErr>>;

#[derive(Debug, Error)]
pub enum CallErr {
    #[error("{0}")]
    FnExecution(String),

    #[error("function parameter declaration")]
    WrongFnParamDecl,

    #[error("{0}")]
    NativeFn(#[from] NativeFnErr),
}

impl RizonReport for CallErr {
    fn get_err_msg(&self) -> String {
        format!("{}: {}", "Call error".red(), self)
    }
}


pub trait Callable {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<RtVal>>>,
    ) -> Result<Rc<RefCell<RtVal>>, RizonResult<CallErr>>;

    fn arity(&self) -> usize;
}
