use frontend::parser::utils::lex_and_parse;

use crate::values::RtVal;
use crate::interpreter::{Interpreter, PhyResInterp};


pub fn lex_parse_interp(code: &str) -> Result<RtVal, PhyResInterp> {
    let nodes = lex_and_parse(code).unwrap();
    let mut interp = Interpreter::new();
    interp.interpret(&nodes)
}
