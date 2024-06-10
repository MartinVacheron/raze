#[cfg(test)]
use crate::{
    expr::Expr,
    interpreter::Interpreter,
    lexer::Lexer,
    parser::Parser,
    results::PhyResult,
    test_parser::{ExprInfos, TestParser},
    values::RuntimeVal,
};

#[cfg(test)]
pub fn lex_and_parse(code: &str) -> Result<Vec<Expr>, Vec<PhyResult>> {
    let mut lexer = Lexer::new();
    let tokens = lexer.tokenize(code).unwrap();
    let mut parser = Parser::default();
    parser.parse(tokens)
}

#[cfg(test)]
pub fn get_nodes_infos(code: &str) -> ExprInfos {
    let nodes = lex_and_parse(code).unwrap();
    let mut test_parser = TestParser::default();
    test_parser.get_all_infos(&nodes).unwrap().clone()
}

#[cfg(test)]
pub fn lex_parse_interp(code: &str) -> Result<RuntimeVal, PhyResult> {
    let nodes = lex_and_parse(code).unwrap();
    let interp = Interpreter {};
    interp.interpret(&nodes)
}

#[cfg(test)]
pub fn produce_runtime_error(code: &str) -> bool {
    use crate::results::PhyResultKind;

    match lex_parse_interp(code) {
        Ok(_) => false,
        Err(e) => match e.kind {
            PhyResultKind::RuntimeErr { .. } => true,
            _ => false
        }
    }
}
