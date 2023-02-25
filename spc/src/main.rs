mod parser;
mod tokenizer;

use tokenizer::tokenize;

fn main() {
    for t in tokenize("Hello, world! == ===") {
        println!("{t}");
    }
}
