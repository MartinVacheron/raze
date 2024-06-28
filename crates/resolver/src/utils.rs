use frontend::parser::utils::lex_and_parse;
use tools::results::PhyResult;

use crate::resolver::{Resolver, ResolverErr};


pub fn lex_parse_resolve(code: &str) -> Result<Resolver, PhyResult<ResolverErr>> {
    let nodes = lex_and_parse(code).unwrap();
    let mut resolver = Resolver::default();
    match resolver.resolve(&nodes) {
        Ok(_) => Ok(resolver),
        Err(e) => Err(e),
    }
}
