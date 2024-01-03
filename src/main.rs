mod asmgen;
mod ast;
mod irgen;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};
use std::io::Result;

lalrpop_mod!(sysy);

struct Mode {
    koopair_only: bool,
}

impl Mode {
    pub fn new(arg: &String) -> Self {
        match arg.as_str() {
            "-koopa" => Self { koopair_only: true },
            _ => Self {
                koopair_only: false,
            },
        }
    }
    pub fn is_koopair_only(&self) -> bool {
        self.koopair_only
    }
}
fn main() -> Result<()> {
    // cargo run -- -koopa hello.c -o hello.koopa
    // cargo run -- -riscv hello.c -o hello.riscv
    let args = args().collect::<Vec<String>>();
    let mode = Mode::new(&args[1]);
    let input = &args[2];
    let output = &args[4];

    let input_file = read_to_string(input).unwrap();
    let ast = sysy::CompUnitParser::new().parse(&input_file).unwrap();
    if mode.is_koopair_only() {
        println!("{:#?}", ast);
    }
    let irprogram = irgen::generate(&ast).unwrap();
    if mode.is_koopair_only() {
        KoopaGenerator::from_path(output)
            .unwrap()
            .generate_on(&irprogram)
            .unwrap();
    } else {
        let mut file = File::create(output).unwrap();
        let _ = asmgen::generate(&irprogram, &mut file);
    }
    Ok(())
}
