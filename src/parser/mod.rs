use lazy_static;
use pest::pratt_parser;
use pest::pratt_parser::PrattParser;
use std::collections::hash_map::Iter;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::default;
use std::env::args;

use crate::ast::CodeBlock;
use crate::ast::Expr;
use crate::ast::FunctionDef;
use crate::ast::InbuiltType;
use crate::ast::Statement;
use crate::ast::Struct_def;
use crate::ast::ZagapType;
use crate::ast::STR2TYPE;

use super::ast::ProgramTable;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "parser/grammar.pest"]
pub struct ZagapParser;

lazy_static::lazy_static! {
    static ref PRATT:PrattParser<Rule> ={
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;
        PrattParser::new()
        .op(Op::postfix(type_change))
        .op(Op::infix(logical, Left))
        .op(Op::infix(binary, Left))
        .op(Op::infix(plus, Left)|Op::infix(minus, Left))
        .op(Op::infix(asterix, Left)|Op::infix(div, Left)|Op::infix(modulo, Left))
        .op(Op::postfix(type_change)|Op::postfix(element)|Op::postfix(index))
    };
}

// #[you_can::turn_off_the_borrow_checker]
pub fn parse_programtable<'a>(code: &'a str) -> ProgramTable {
    //let mut ret;
    let mut res = ProgramTable::default();
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
                let name = tok.as_str();
                res.str2struct.insert(name, res.structs.len());
                res.structs.push(super::ast::Struct_def::default());
            }
            _ => {}
        }
    }
    //----------- Second pass structs -----------//
    for i in ast.clone().into_inner() {
        match i.as_rule() {
            Rule::import => {}
            Rule::struct_def => {
                let (name, v) = parse_struct(i, &res);
                res.structs[res.str2struct[name]] = v;
            }
            _ => {}
        }
    }
    //------------First pass funcs--------------//
    for i in ast.clone().into_inner() {
        match i.as_rule() {
            Rule::import => {}
            Rule::function_def => {
                let mut it = i.into_inner();
                let mut tok = it.next().unwrap();
                if tok.as_rule() == Rule::exportk {
                    tok = it.next().unwrap();
                }
                res.str2func.insert(tok.as_str(), res.funcs.len());
                res.funcs.push(FunctionDef::default());
            }
            Rule::function_import => {
                let mut it = i.into_inner();
                let mut val = FunctionDef::default();
                val.import = true;
                let mut name = it.next().unwrap().as_str();
                val.export_name = Some(name);
                for i in it {
                    match i.as_rule() {
                        Rule::arg => val.args.push(parse_arg(i, &res)),
                        Rule::identifier => name = i.as_str(),
                        Rule::r#type => val.ret = parse_type(i, &res),
                        _ => unreachable!(),
                    }
                }
                res.str2func.insert(name, res.funcs.len());
                res.funcs.push(val);
            }
            _ => {}
        }
    } //----------Second pass funcs----------//
    for i in ast.into_inner() {
        match i.as_rule() {
            Rule::function_def => {
                let (name, fnc) = parse_function_def(i, &res);
                res.funcs[res.str2func[name]] = fnc;
            }
            _ => {}
        }
    }
    todo!()
}

fn parse_function_def<'a>(ast: Pair<'a, Rule>, table: &ProgramTable) -> (&'a str, FunctionDef<'a>) {
    let mut it = ast.into_inner();
    let mut tok = it.next().unwrap();
    let mut val = FunctionDef::default();
    if tok.as_rule() == Rule::exportk {
        tok = it.next().unwrap();
        val.export_name = Some(tok.as_str());
    }
    let name = tok.as_str();
    for i in it {
        match i.as_rule() {
            Rule::arg => val.args.push(parse_arg(i, &table)),
            Rule::identifier => val.export_name = Some(i.as_str()),
            Rule::code_block => val.code = parse_code_block(i, &table, 0, None),
            _ => unreachable!(),
        }
    }
    (name, val)
}

fn parse_code_block<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    var_namer: usize,
    var_lookup: Option<&BTreeMap<&'a str, usize>>,
) -> CodeBlock<'a> {
    let mut res = CodeBlock::default();
    for i in ast.into_inner() {
        res.statements
            .push(parse_statement(i, &table, &mut res.vars, var_namer, None))
    }
    res
}

fn parse_statement<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    c_var: &mut BTreeMap<&'a str, (usize, ZagapType)>,
    var_namer: usize,
    var_lookup: Option<&BTreeMap<&'a str, usize>>,
) -> Statement<'a> {
    let mut it = ast.into_inner();
    todo!()
}

fn parse_expr<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    c_var: &BTreeMap<&'a str, (usize, ZagapType)>,
) -> Expr<'a> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::func_call => {
                let mut it = primary.into_inner();
                let name = it.next().unwrap().as_str();
                Expr::EFuncCall(
                    table.str2func[name],
                    it.map(|e| parse_expr(e, &table, &c_var)).collect(),
                )
            }
            Rule::identifier => Expr::EVar(table.str2func[primary.as_str()]),
            Rule::literals => {
                let it = primary.into_inner();
                match it.peek().unwrap().as_rule() {
                    Rule::char_literal => unimplemented!(),
                    Rule::string_literal => Expr::Eslit(it.as_str()),
                    Rule::string_literal => Expr::Enlit(it.as_str().parse().expect("not a number")),
                    _ => unreachable!(),
                }
            }
            Rule::expr => parse_expr(primary, &table, &c_var),
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            _ => unimplemented!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            _ => unimplemented!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            _ => unimplemented!(),
        })
        .parse(ast.into_inner())
}

fn parse_arg<'a>(ast: Pair<'a, Rule>, table: &ProgramTable) -> (&'a str, ZagapType) {
    let mut iter = ast.into_inner();
    let name = iter.next().unwrap().as_str();
    let t = parse_type(iter.next().unwrap(), table);
    (name, t)
}

fn parse_type<'a>(ast: Pair<Rule>, table: &'a ProgramTable) -> ZagapType {
    let inner = ast.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::identifier => {
            if let Some(res) = STR2TYPE.get(inner.as_str()) {
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

fn parse_struct<'a>(ast: Pair<'a, Rule>, table: &ProgramTable<'a>) -> (&'a str, Struct_def<'a>) {
    let mut res = Struct_def::default();
    let mut it = ast.into_inner();
    let mut val = it.next().unwrap();
    if val.as_rule() == Rule::exportk {
        val = it.next().unwrap();
        res.export_name = Some(val.as_str());
    }
    let name = val.as_str();
    val = it.next().unwrap();
    match val.as_rule() {
        Rule::identifier => {
            res.export_name = Some(val.as_str());
        }
        Rule::arg => {
            it.next_back().unwrap();
        }
        _ => unreachable!(),
    }
    for i in it {
        let (name, t) = parse_arg(i, &table);
        res.elements.insert(name, t);
    }
    (name, res)
}
