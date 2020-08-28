use rlox::InterpretResult;

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::process::exit;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() >= 2 {
        let debug = (args.len() == 3) && args[2].eq("--debug");
        let result = run_file(args.get(1).unwrap(), debug);
        exit(match result {
            InterpretResult::InterpretOK => 0,
            InterpretResult::InterpretCompileError => 65,
            InterpretResult::InterpretRuntimeError => 70,
        })
    } else {
        println!("Usage: rlox path [--debug]");
    }
}

fn run_file(filename: &String, debug: bool) -> InterpretResult {
    let path = Path::new(&filename);
    let path_display = path.display();

    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(why) => {
            eprintln!("Failed to open {}: {}", path_display, why);
            exit(1);
        }
    };

    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Ok(_) => return rlox::interpret(&s, debug, false),
        Err(why) => {
            eprintln!("Failed to read {}: {}", path_display, why);
            exit(1);
        }
    };
}
