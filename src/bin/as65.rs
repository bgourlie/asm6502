use clap::Parser;
use nom::error::convert_error;
use std::io::{self, Write};

use asm6502::assemble;
use asm6502::AssembleError;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    filename: String,
    output: Option<String>,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let path = std::path::Path::new(&args.filename);
    let asm = std::fs::read_to_string(path)?;
    let input = asm.as_str();
    let mut buf = Vec::<u8>::new();
    match assemble(input, &mut buf) {
        Err(AssembleError::TokenizeError(err)) => {
            match err {
                nom::Err::Incomplete(needed) => {
                    println!("Incomplete assembly: {:#?}", needed);
                }
                nom::Err::Failure(err) => {
                    println!("Failed to tokenize input: {}", convert_error(input, err));
                }
                nom::Err::Error(err) => {
                    println!("Failed to tokenize input: {}", convert_error(input, err));
                }
            }
            std::process::exit(1);
        }
        Err(AssembleError::ParseError(msg)) => {
            println!("Failed to parse input: {msg}");
            std::process::exit(1);
        }
        Err(err) => {
            println!("Failed to assemble: {err:?}");
            std::process::exit(1);
        }
        Ok(_) => {}
    }

    match args.output {
        Some(output) => {
            let path = std::path::Path::new(&output);

            println!("Assembly:\n{:?}", buf);
            std::fs::write(path, &buf)?;
        }
        None => {
            io::stdout().write_all(&buf)?;
        }
    }

    Ok(())
}
