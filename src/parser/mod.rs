use crate::ast::BinaryOp;
use crate::ast::CodeBlock;
use crate::ast::Expr;
use crate::ast::FunctionDef;
use crate::ast::InbuiltType;
use crate::ast::Statement;
use crate::ast::StructDef;
use crate::ast::UnaryOp;
use crate::ast::ZagapType;
use crate::ast::STR2TYPE;
use lazy_static;
use pest::pratt_parser::PrattParser;
use std::collections::BTreeMap;

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
        .op(Op::prefix(dref)|Op::prefix(to_ptr)|Op::prefix(lnot)|Op::prefix(bnot))
        .op(Op::postfix(element)|Op::postfix(index))
    };
}

pub fn parse_programtable<'a>(code: &'a str) -> ProgramTable {
    //let mut ret;
    let mut res = ProgramTable::default();
    let ast = ZagapParser::parse(Rule::program, code)
        .expect("Unable to parse code with error:{ast:#?}")
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
                res.structs.push(super::ast::StructDef::default());
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
    res
}

fn parse_function_def<'a>(ast: Pair<'a, Rule>, table: &ProgramTable) -> (&'a str, FunctionDef<'a>) {
    let mut it = ast.into_inner();
    let mut tok = it.next().unwrap();
    let mut val = FunctionDef::default();
    let mut btree = BTreeMap::new();
    if tok.as_rule() == Rule::exportk {
        tok = it.next().unwrap();
        val.export_name = Some(tok.as_str());
    }
    let mut namer = 0;
    let name = tok.as_str();
    if name == "main" {
        val.export_name = Some(name);
    }
    for i in it {
        match i.as_rule() {
            Rule::r#type => val.ret = parse_type(i, table),
            Rule::arg => {
                let arg = parse_arg(i, table);
                btree.insert(arg.0, (namer, arg.1.clone()));
                val.args.push(arg);
                namer += 1;
            }
            Rule::identifier => val.export_name = Some(i.as_str()),
            Rule::code_block => val.code = parse_code_block(i, &table, namer, &btree),
            _ => {
                unreachable!()
            }
        }
    }
    (name, val)
}

fn parse_code_block<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    mut var_namer: usize,
    var_lookup: &BTreeMap<&'a str, (usize, ZagapType)>,
) -> CodeBlock<'a> {
    let mut res = CodeBlock::default();
    let mut lookup = var_lookup.clone();
    for i in ast.into_inner() {
        let (st, dec) = parse_statement(i, table, &lookup, var_namer);
        if let Some(dec) = dec {
            var_namer += 1;
            res.vars.insert(dec.0, (dec.1, dec.2.clone()));
            lookup.insert(dec.0, (dec.1, dec.2));
        }
        res.statements.push(st);
    }
    res
}

fn parse_statement<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    c_var: &BTreeMap<&'a str, (usize, ZagapType)>,
    var_namer: usize,
) -> (Statement<'a>, Option<(&'a str, usize, ZagapType)>) {
    let mut it = ast.into_inner();
    let val = it.next().unwrap();
    match val.as_rule() {
        Rule::ret => (
            Statement::Ret(parse_expr(val.into_inner().next().unwrap(), table, c_var)),
            None,
        ),
        Rule::brk => (Statement::Brk, None),
        Rule::cti => (Statement::Cti, None),
        Rule::if_block => {
            let mut it = val.into_inner();
            let cond = parse_expr(it.next().unwrap(), table, c_var);
            let ftrue = Statement::Code(parse_code_block(
                it.next().unwrap(),
                table,
                var_namer,
                c_var,
            ));
            (
                Statement::IfBlock {
                    cond: cond,
                    ftrue: Box::from(ftrue),
                    ffalse: it.next().map(|x| {
                        Box::from(match x.as_rule() {
                            Rule::code_block => {
                                Statement::Code(parse_code_block(x, table, var_namer, c_var))
                            }
                            Rule::wraper_if => parse_statement(x, table, c_var, var_namer).0,
                            _ => unreachable!(),
                        })
                    }),
                },
                None,
            )
        }
        Rule::for_while => {
            let mut it = val.into_inner();
            let cond = parse_expr(it.next().unwrap(), table, c_var);
            (
                Statement::Cwhile(
                    cond,
                    parse_code_block(it.next().unwrap(), table, var_namer, c_var),
                ),
                None,
            )
        }
        Rule::for_c => {
            let mut it = val.into_inner();
            let first = parse_statement(it.next().unwrap(), table, c_var, var_namer);
            let mut var = c_var.clone();
            if let Some(v) = &first.1 {
                var.insert(v.0, (v.1, v.2.clone()));
            }
            let second = parse_expr(it.next().unwrap(), table, &var);
            let third = parse_statement(it.next().unwrap(), table, &var, var_namer + 1);
            let code = parse_code_block(it.next().unwrap(), table, var_namer + 2, &var);
            (
                Statement::Code(CodeBlock {
                    statements: Vec::from([Statement::Cfor {
                        init: Box::new(first.0),
                        cond: second,
                        post: Box::new(third.0),
                        code: code,
                    }]),
                    vars: if let Some(v) = first.1 {
                        BTreeMap::from([(v.0, (v.1, v.2))])
                    } else {
                        BTreeMap::new()
                    },
                }),
                None,
            )
        }
        Rule::for_loop => (
            Statement::Cloop(parse_code_block(
                val.into_inner().next().unwrap(),
                table,
                var_namer,
                c_var,
            )),
            None,
        ),
        Rule::code_block => (
            Statement::Code(parse_code_block(val, table, var_namer, c_var)),
            None,
        ),
        Rule::assigment => {
            let mut it = val.into_inner();
            let first = parse_expr(it.next().unwrap(), table, c_var);
            let second = parse_expr(it.next().unwrap(), table, c_var);
            (Statement::Assigment(first, second), None)
        }
        Rule::expr => (Statement::Expr(parse_expr(val, table, c_var)), None),
        Rule::declaration => {
            let mut it = val.into_inner();
            let name = it.next().unwrap().as_str();
            let val = it.next().unwrap();
            let t = match val.as_rule() {
                Rule::array_def => todo!(),
                Rule::r#type => parse_type(val, table),
                _ => unreachable!(),
            };
            (Statement::None, Some((name, var_namer, t)))
        }
        Rule::declarationinit => {
            let mut it = val.clone().into_inner();
            it.next();
            if let (_, Some(var)) = parse_statement(val, table, c_var, var_namer) {
                (
                    Statement::Assigment(
                        Box::from(Expr::EVar(var.1)),
                        parse_expr(it.next().unwrap(), table, c_var),
                    ),
                    Some(var),
                )
            } else {
                unreachable!()
            }
        }
        _ => unreachable!(),
    }
}

fn parse_expr<'a>(
    ast: Pair<'a, Rule>,
    table: &ProgramTable,
    c_var: &BTreeMap<&'a str, (usize, ZagapType)>,
) -> Box<Expr<'a>> {
    PRATT
        .map_primary(|primary| {
            Box::from(match primary.as_rule() {
                Rule::func_call => {
                    let val = primary.into_inner().next().unwrap();
                    match val.as_rule() {
                        Rule::func_call_ => {
                            let mut it = val.into_inner();
                            let name = it.next().unwrap().as_str();
                            Expr::EFuncCall(
                                *table
                                    .str2func
                                    .get(name)
                                    .unwrap_or_else(|| panic!("Unknown function `{name}`")),
                                it.map(|e| parse_expr(e, &table, &c_var)).collect(),
                            )
                        }
                        Rule::size_ofe => {
                            let tp = parse_type(val.into_inner().next().unwrap(), table);
                            Expr::Enlit(sizeof_zagaptype(&tp, table) as f64)
                        }
                        _ => unreachable!(),
                    }
                }
                Rule::identifier => Expr::EVar(
                    c_var
                        .get(primary.as_str())
                        .unwrap_or_else(|| {
                            panic!("Unknown variable {name}", name = primary.as_str())
                        })
                        .0,
                ),
                Rule::literals => {
                    let it = primary.into_inner();
                    match it.peek().unwrap().as_rule() {
                        Rule::char_literal => Expr::EClit(it.as_str()),
                        Rule::string_literal => Expr::Eslit(it.as_str()),
                        Rule::number_literal => {
                            Expr::Enlit(it.as_str().parse().expect("not a number"))
                        }
                        _ => unreachable!(),
                    }
                }
                Rule::expr => *parse_expr(primary, &table, &c_var),
                _ => {
                    unreachable!()
                }
            })
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::dref => Box::from(Expr::EDref(rhs)),
            Rule::to_ptr => Box::from(Expr::EPtr(rhs)),
            Rule::minus => Box::from(Expr::EUnary {
                op: UnaryOp::Minus,
                val: rhs,
            }),
            Rule::lnot => Box::from(Expr::EUnary {
                op: UnaryOp::Lnot,
                val: rhs,
            }),
            Rule::bnot => Box::from(Expr::EUnary {
                op: UnaryOp::Bnot,
                val: rhs,
            }),
            _ => unimplemented!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::index => Box::from(Expr::EIndex {
                lhs: lhs,
                rhs: parse_expr(op.into_inner().next().unwrap(), table, c_var),
            }),
            Rule::element => Box::from(Expr::EElement(
                lhs,
                op.into_inner().next().unwrap().as_str(),
            )),
            Rule::type_change => Box::from(Expr::Etypechange(
                lhs,
                parse_type(op.into_inner().next().unwrap(), table),
            )),
            _ => unimplemented!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::plus => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: BinaryOp::Plus,
            }),
            Rule::minus => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: BinaryOp::Minus,
            }),
            Rule::asterix => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: BinaryOp::Asterix,
            }),
            Rule::div => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: BinaryOp::Div,
            }),
            Rule::modulo => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: BinaryOp::Modulo,
            }),
            Rule::binary => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: match op.into_inner().next().unwrap().as_rule() {
                    Rule::band => BinaryOp::Band,
                    Rule::bor => BinaryOp::Bor,
                    Rule::bxor => BinaryOp::Bxor,
                    Rule::bshiftr => BinaryOp::Bshiftr,
                    Rule::bshiftl => BinaryOp::Bshiftl,
                    _ => unreachable!(),
                },
            }),
            Rule::logical => Box::from(Expr::EBinary {
                lhs: lhs,
                rhs: rhs,
                op: match op.into_inner().next().unwrap().as_rule() {
                    Rule::leq => BinaryOp::Leq,
                    Rule::lless => BinaryOp::Lless,
                    Rule::llesseq => BinaryOp::Llesseq,
                    Rule::lmore => BinaryOp::Lmore,
                    Rule::lmoreeq => BinaryOp::Lmoreeq,
                    Rule::lneq => BinaryOp::Lneq,
                    Rule::land => BinaryOp::Land,
                    Rule::lor => BinaryOp::Lor,
                    _ => unreachable!(),
                },
            }),
            _ => unreachable!(),
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
        Rule::ptr_type => ZagapType::Ptr(Box::new(parse_type(
            inner.into_inner().next().unwrap(),
            table,
        ))),
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

fn parse_struct<'a>(ast: Pair<'a, Rule>, table: &ProgramTable<'a>) -> (&'a str, StructDef<'a>) {
    let mut res = StructDef::default();
    let mut it = ast.into_inner();
    let mut val = it.next().unwrap();
    if val.as_rule() == Rule::exportk {
        val = it.next().unwrap();
        res.export_name = Some(val.as_str());
    }
    let name = val.as_str();
    let tmp = it.peek();
    if tmp.is_none() {
        return (name, res);
    }
    val = tmp.unwrap();
    match val.as_rule() {
        Rule::identifier => {
            res.export_name = Some(val.as_str());
            it.next();
        }
        Rule::arg => {}
        _ => unreachable!(),
    }
    for i in it {
        let (name, t) = parse_arg(i, &table);
        res.elements.insert(name, t);
    }
    (name, res)
}

fn sizeof_zagaptype(tp: &ZagapType, table: &ProgramTable) -> usize {
    match tp {
        ZagapType::Ptr(_) => 8,
        ZagapType::Array { t, size } => size * sizeof_zagaptype(t, table),
        ZagapType::Struct(id) => table.structs[*id]
            .elements
            .iter()
            .map(|x| sizeof_zagaptype(x.1, table))
            .sum(),
        ZagapType::Inbuilt(t) => match t {
            InbuiltType::Void => 0,
            InbuiltType::Bool | InbuiltType::I8 | InbuiltType::U8 | InbuiltType::C8 => 1,
            InbuiltType::I16 | InbuiltType::U16 | InbuiltType::C16 => 2,
            InbuiltType::I32 | InbuiltType::U32 | InbuiltType::C32 | InbuiltType::F32 => 4,
            InbuiltType::I64
            | InbuiltType::U64
            | InbuiltType::F64
            | InbuiltType::USIZE
            | InbuiltType::ISIZE => 8,
            InbuiltType::F128 => 16,
        },
    }
}
