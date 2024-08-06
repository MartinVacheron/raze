use clap::Parser as ClapParser;
use std::{
    error::Error,
    fs,
    io::{self, Write},
    process,
};
use colored::*;

use rizon_frontend::{lexer::Lexer, parser::Parser};
use rizon_static_analyzer::StaticAnalyzer;
use rizon_runtime::{interpreter::Interpreter, values::RtVal};

// --------
//   Cli
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about = "Interpreter for Rizon language")]
struct Cli {
    #[arg(short, long, default_value_t = String::from("placeholder.rz"))]
    /// Path to the file to parse
    file: String,

    /// Interactive mode after interpreting a file
    #[arg(short, long)]
    inter: bool,

    // Prints the tokens
    #[arg(long)]
    print_tokens: bool,

    // Static analysis
    #[arg(short, long)]
    static_analyse: bool,
}

struct Repl {
    cli: Cli,
    static_analyzer: StaticAnalyzer,
    interpreter: Interpreter,
}

fn main() {
    let mut repl = Repl {
        cli: Cli::parse(),
        static_analyzer: StaticAnalyzer::default(),
        interpreter: Interpreter::new(),
    };

    repl.run();
}

impl Repl {
    pub fn run(&mut self) {
        let _ = match self.cli.file.as_str() {
            "placeholder.rz" => self.run_repl(),
            f => self.run_file(f.into()),
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

        println!("\n  {}", "Rizon language interpreter v0.0\n".yellow());

        loop {
            input.clear();
            print!("> ");
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
                    .for_each(|e| e.report(&self.cli.file, &code));

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
                    .for_each(|e| e.report(&self.cli.file, &code));
                
                return
            }
        };

        let locals = match self.static_analyzer.resolve(&nodes) {
            Ok(l) => l,
            Err(e) => {
                e.iter()
                    .for_each(|e| e.report(&self.cli.file, &code));

                return
            }
        };

        if !self.cli.static_analyse {
            match self.interpreter.interpret(&nodes, locals) {
                Ok(res) => {
                    if *res.borrow() != RtVal::Null {
                        println!("{}", *res.borrow());
                    }
                }
                Err(e) => e.report(&self.cli.file, &code),
            }
        }
    }
}
