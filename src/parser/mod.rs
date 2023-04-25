use std::collections::hash_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::default;
use std::env::args;

use crate::ast::str2type;
use crate::ast::InbuiltType;
use crate::ast::Struct_def;
use crate::ast::ZagapType;

use super::ast::ProgramTable;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"] // relative to project `src`
pub struct ZagapParser;

pub fn parse_programtable<'a>(code: &'a str) -> ProgramTable {
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
    //----------- first pass structs ------------//
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
    }
    //----------- Second pass structs -----------//
    for i in ast.clone().into_inner() {
        match i.as_rule() {
            Rule::struct_def => {
                let (name, v) = parse_struct(i, &res);
                res.structs[res.str2struct[name]] = v;
            }
            _ => {}
        }
    }
    for i in ast.clone().into_inner() {
        match i.as_rule() {
            Rule::function_def => {
                let mut it = i.into_inner();
                let mut fnc: super::ast::FunctionDef = super::ast::FunctionDef::default();
                fnc.import = true;
                let name = it.next().unwrap().as_str();
                for i in it {
                    match i.as_rule() {
                        Rule::identifier => {
                            fnc.export_name = Some(i.as_str());
                        }
                        Rule::arg => {
                            let mut it = i.into_inner();
                            let name = it.next().unwrap().as_str();
                            fnc.args.push((name, parse_type(it.next().unwrap(), &res)));
                        }
                        _ => unreachable!(),
                    }
                }
            }
            _ => {}
        }
    } //----------Third pass----------//
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
    todo!()
}

fn parse_arg<'a>(ast: Pair<'a, Rule>, table: &'a ProgramTable) -> (&'a str, ZagapType) {
    let mut iter = ast.into_inner();
    let name = iter.next().unwrap().as_str();
    let t = parse_type(iter.next().unwrap(), table);
    (name, t)
}

fn parse_type<'a>(ast: Pair<Rule>, table: &'a ProgramTable) -> ZagapType {
    let inner = ast.into_inner().next().unwrap();
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

fn parse_struct<'a>(ast: Pair<'a, Rule>, table: &'a ProgramTable) -> (&'a str, Struct_def<'a>) {
    let mut res = Struct_def::default();
    let mut it = ast.into_inner();
    let mut exp = false;
    let mut val = it.next().unwrap();
    if val.as_rule() == Rule::exportk {
        exp = true;
        val = it.next().unwrap();
    }
    let name = val.as_str();
    if exp {
        res.export_name = Some(it.next().unwrap().as_str());
    }
    for i in it {
        let (name, t) = parse_arg(i, &table);
        res.elements.insert(name, t);
    }
    (name, res)
}
