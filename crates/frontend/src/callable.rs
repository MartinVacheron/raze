use crate::{interpreter::Interpreter, results::{PhyReport, PhyResult}, values::RtVal};

pub trait Callable<T: PhyReport> {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: Vec<RtVal>,
    ) -> Result<RtVal, PhyResult<T>>;

    fn arity(&self) -> usize;
}
