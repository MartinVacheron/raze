use tools::results::{PhyReport, PhyResult};

use crate::{interpreter::Interpreter, values::RtVal};

pub trait Callable<T: PhyReport> {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<RtVal>,
    ) -> Result<RtVal, PhyResult<T>>;

    fn arity(&self) -> usize;
}
