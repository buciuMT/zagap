use std::collections::hash_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;

use crate::ast::str2type;
use crate::ast::InbuiltType;
use crate::ast::ZagapType;

use super::ast::ProgramTable;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"] // relative to project `src`
pub struct ZagapParser;

pub fn parse_ProgramTable<'a>(code: &'a str) -> ProgramTable {
    //let mut ret;
    let mut res = ProgramTable {
        structs: Vec::new(),
        funcs: Vec::new(),
        str2func: HashMap::new(),
        str2struct: HashMap::new(),
    };
    let ast = ZagapParser::parse(Rule::program, code)
        .expect("Unable to parse code with error:{ast}")
        .next()
        .unwrap();
    //----------- first pass ------------//
    for i in ast.clone().into_inner() {
        match i.as_rule() {
            Rule::struct_def => {
                let mut it = i.into_inner();
                let mut tok = it.next().unwrap();
                if tok.as_rule() == Rule::exportk {
                    tok = it.next().unwrap();
                }
                res.str2func.insert(it.as_str(), res.structs.len());
                res.structs.push(super::ast::Struct_def::default());
            }
            _ => {}
        }
    } //----------Second pass----------//
    for i in ast.into_inner() {
        match i.as_rule() {
            Rule::import => {}
            Rule::function_import => {}
            Rule::function_def => {}
            Rule::struct_def => {
                println!("{i:#?}")
            }
            _ => unreachable!(),
        }
    }

    //todo!();
    res
}

impl<'a> super::ast::Struct_def<'a> {
    fn parse(ast: Pair<Rule>, table: &'a ProgramTable) -> (&'a str, super::ast::Struct_def<'a>) {
        let mut res: super::ast::Struct_def<'a>;
        let mut name: &str;
        let mut export: Option<&str> = None;
        let mut iter = ast.into_inner();
        let mut tok = iter.next().unwrap();
        if tok.as_rule() == Rule::exportk {
            tok = iter.next().unwrap();
            export = Some(tok.as_str());
        }
        name = tok.as_str();
        for i in iter {
            match i.as_rule() {
                Rule::identifier => {
                    export = Some(i.as_str());
                }
                Rule::arg => {}
                _ => unreachable!(),
            }
        }
        todo!()
    }
}

fn parse_arg<'a>(ast: Pair<'a, Rule>, table: &'a ProgramTable) -> (&'a str, ZagapType) {
    let mut iter = ast.into_inner();
    let name = iter.next().unwrap().as_str();
    let t = parse_type(iter.next().unwrap(), table);
    (name, t)
}

fn parse_type<'a>(ast: Pair<Rule>, table: &'a ProgramTable) -> ZagapType {
    let mut inner = ast.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::identifier => {
            if let Some(res) = str2type.get(inner.as_str()) {
                ZagapType::Inbuilt(*res)
            } else if let Some(res) = table.str2struct.get(inner.as_str()) {
                ZagapType::Struct(*res)
            } else {
                panic!("Unable to find type `{tp}`", tp = inner.as_str())
            }
        }
        Rule::ptr_type => ZagapType::Ptr(Box::new(parse_type(inner, table))),
        Rule::array_type => {
            let mut it = inner.into_inner();
            let t = parse_type(it.next().unwrap(), table);
            let size: usize = it
                .next()
                .unwrap()
                .as_str()
                .parse()
                .expect("Life has no meaning");
            ZagapType::Array {
                t: Box::new(t),
                size: size,
            }
        }
        _ => unreachable!(),
    }
}
