use crate::lexer::LexResult;

mod codegen;
mod lexer;
mod parser;
mod semantics;
mod token;

fn main() {
    let input = r#"
            // this is a line comment
            1 + 3 = 4;
            var x = 0;
            val y = 4.20;
            if(x == y){ "!mann"; }
        "#;
    println!("Input: {input:#}");
    let LexResult { tokens, errors } = lexer::lex_with_errors(input);
    for e in errors.iter() {
        eprintln!("{e:#?}");
    }
    if !errors.is_empty() {
        std::process::exit(1);
    }

    let ast = match parser::parse_source(tokens) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {e:#?}");
            std::process::exit(1);
        }
    };
    println!("AST: {ast:#?}")

    // if let Err(err) = semantics::check(&ast) {
    //     eprintln!("Semantic error: {err:#?}");
    //     std::process::exit(1);
    // }

    //TODO codegen
}
