use std::cell::RefCell;
use std::rc::Rc;

use resolver::utils::lex_parse_resolve;
use frontend::parser::utils::lex_and_parse;

use crate::values::RtVal;
use crate::interpreter::{Interpreter, RevResInterp};


pub fn lex_parse_resolve_interp(code: &str) -> Result<Rc<RefCell<RtVal>>, RevResInterp> {
    let locals = lex_parse_resolve(code).unwrap();
    let nodes = lex_and_parse(code).unwrap();
    let mut interp = Interpreter::new();
    interp.interpret(&nodes, locals)
}
