use crate::lexer::LexResult;

mod codegen;
mod lexer;
mod parser;
mod semantics;
mod token;

fn main() {
    let input = r#"
            // this is a line comment
            if(x){ "!mann" }
        "#;
    println!("Input: {input:#}");
    let LexResult { tokens, errors } = lexer::lex_with_errors(&input);
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

    if let Err(err) = semantics::check(&ast) {
        eprintln!("Semantic error: {err:#?}");
        std::process::exit(1);
    }

    //TODO codegen
}
