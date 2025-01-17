mod compile;
mod parse;

use std::fs;

use compile::qbe::compile;
use parse::grammar::{PL0Parser, Rule};
use pest_consume::Parser;

fn main() {
    let file = fs::read_to_string("test.pl0").unwrap();

    let inputs = PL0Parser::parse(Rule::program, &file).unwrap();

    let input = inputs.single().unwrap();

    let module = compile(&PL0Parser::program(input.clone()).unwrap());

    println!("{:#?}", PL0Parser::program(input).unwrap());
    println!("{}", module);
}
