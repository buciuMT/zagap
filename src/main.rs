mod ast;
mod importer;
mod parser;
use parser::{ZagapParser, parse_programtable};
use pest::Parser;

fn main() {
    let x = parse_programtable(
        "import IO
        export struct name as exp{
            ast:i32,
            bxa:i64,
        }
        ",
    );
    //print!("{x:#?}");
}
