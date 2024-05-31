use std::{
    error::Error, fs, io::{self, Write}, process
};
use clap::Parser as ClapParser;

use frontend::{lexer::Lexer, parser::Parser, ast_pretty_print::AstPrinter};


// --------
//   Cli
// --------

#[derive(ClapParser)]
#[command(version)]
#[command(about="Interpreter for Raze language")]
struct Cli {
    #[arg(short, long)]
    /// Path to the file to parse
    file: Option<String>,

    /// Interactive mode after interpreting a file
    #[arg(short, long)]
    inter: bool,

    // Prints the AST tree
    // #[arg(short, long)]
    // ast_print: bool,
}

fn main() {
    let cli = Cli::parse();

    let _ = match cli.file {
        Some(f) => run_file(f),
        None => run_repl()
    };
}

fn run(code: String) {
    let mut lexer = Lexer::new(&code);
    
    let tokens = match lexer.tokenize() {
        Ok(tk) => tk,
        Err(e) => {
            e.iter().for_each(|e| e.report(&"placeholder.rz".into(), &code));
            return
        }
    };

    let mut parser = Parser::new(&tokens);
    let nodes = match parser.parse() {
        Ok(n) => n,
        Err(e) => {
            e.iter().for_each(|e| e.report(&"placeholder.rz".into(), &code));
            return
        }
    };

    let ast_print = AstPrinter {};
    for n in nodes {
        println!("{}", ast_print.print(n).unwrap());
    }
}


fn run_file(file_path: String) -> Result<() , Box<dyn Error>> {
    let code = fs::read_to_string(file_path)?;
    run(code);

    Ok(())
}

fn run_repl() -> Result<() , Box<dyn Error>> {
    // Local variables
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();

    loop {
        input.clear();
        print!("\n> ");
        stdout.flush().unwrap();

        stdin.read_line(&mut input)?;
        let trimmed_input = input.trim();

        if trimmed_input == "quit" {
            process::exit(0);
        }

        if input.is_empty() { continue }

        // Execute interpreter
        run(trimmed_input.to_string());
    }
}
