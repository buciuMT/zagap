mod ast;
mod importer;
mod parser;
use parser::{ZagapParser, parse_programtable};
use pest::Parser;

fn main() {
    let x = /*ZagapParser::parse(parser::Rule::program,"import IO
    struct name{
        ast:i32,
        bxa:i64,
    }
    ");*/parse_programtable(
        "
        func main:->i32{
            ret two:;
        }

        func two:->i32{
            ret 2;
        }
        ",
    );
    print!("{x:#?}");
}
