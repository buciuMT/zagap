mod ast;
mod importer;
mod parser;
use parser::{ZagapParser, parse_ProgramTable};
use pest::Parser;

fn main() {
    let x = parse_ProgramTable(
        "import IO
        export struct name as exp{
            ast:i32,
            bxa:i64,
        }
        ",
    );
    //print!("{x:#?}");
}
