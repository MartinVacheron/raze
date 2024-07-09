pub mod resolver;
// Allow to use it as resolver::Resolver instead of resolver::resolver::Resolver
pub use resolver::Resolver;

#[cfg(test)]
mod utils;

extern crate frontend;
