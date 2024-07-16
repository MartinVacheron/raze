use clap::Parser as ClapParser;
use std::{
    error::Error,
    fs,
    io::{self, Write},
    process,
};
use colored::*;

use frontend::{
    ast::ast_pretty_print::AstPrinter, lexer::Lexer, parser::Parser,
};
use resolver::Resolver;
use runtime::{interpreter::Interpreter, values::RtVal};

// --------
//   Cli
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about = "Interpreter for Phy language")]
struct Cli {
    #[arg(short, long)]
    /// Path to the file to parse
    file: Option<String>,

    /// Interactive mode after interpreting a file
    #[arg(short, long)]
    inter: bool,

    // Prints the tokens
    #[arg(long)]
    print_tokens: bool,

    // Prints the AST tree
    #[arg(short, long)]
    print_ast: bool,
}

struct Repl {
    cli: Cli,
    ast_printer: AstPrinter,
    interpreter: Interpreter,
}

fn main() {
    let mut repl = Repl {
        cli: Cli::parse(),
        ast_printer: AstPrinter {},
        interpreter: Interpreter::new(),
    };

    repl.run();
}

impl Repl {
    pub fn run(&mut self) {
        let _ = match &self.cli.file {
            Some(f) => self.run_file(f.clone()),
            None => self.run_repl(),
        };
    }

    fn run_file(&mut self, file_path: String) -> Result<(), Box<dyn Error>> {
        let code = fs::read_to_string(file_path)?;
        self.sequence(code);

        Ok(())
    }

    fn run_repl(&mut self) -> Result<(), Box<dyn Error>> {
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        let mut input = String::new();

        println!("\n  {}", "Rev language interpreter v0.0".yellow());

        loop {
            input.clear();
            print!("\n> ");
            stdout.flush().unwrap();

            stdin.read_line(&mut input)?;
            let trimmed_input = input.trim();

            if trimmed_input == "quit" {
                process::exit(0);
            }

            if trimmed_input.is_empty() {
                continue;
            }

            // Execute interpreter
            self.sequence(trimmed_input.to_string());
        }
    }

    fn sequence(&mut self, code: String) {
        let mut lexer = Lexer::new();
        let mut parser = Parser::default();

        let tokens = match lexer.tokenize(&code) {
            Ok(tk) => tk,
            Err(e) => {
                e.iter()
                    .for_each(|e| e.report(&"placeholder.rz".into(), &code));

                return
            }
        };

        if self.cli.print_tokens {
            println!("Tokens: {:#?}", tokens);
        }

        let nodes = match parser.parse(tokens) {
            Ok(n) => n,
            Err(e) => {
                e.iter()
                    .for_each(|e| e.report(&"placeholder.rz".into(), &code));
                
                return
            }
        };

        if self.cli.print_ast {
            for n in &nodes {
                println!("{}", self.ast_printer.print(n).unwrap());
            }
        }

        let mut resolver = Resolver::default();

        let locals = match resolver.resolve(&nodes) {
            Ok(l) => l,
            Err(e) => {
                e.iter()
                    .for_each(|e| e.report(&"placeholder.rz".into(), &code));

                return
            }
        };

        match self.interpreter.interpret(&nodes, locals) {
            Ok(res) => {
                if *res.borrow() != RtVal::Null {
                    println!("{}", *res.borrow());
                }
            }
            Err(e) => e.report(&"placeholder.rz".into(), &code),
        }
    }
}
