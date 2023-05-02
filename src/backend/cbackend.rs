use ::std::io;

use crate::ast::*;

pub trait CGen {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()>;
}
impl CGen for ProgramTable<'_> {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        for i in &self.str2struct {
            write!(writer, "typedef struct ")?;
            name_of_struct(*i.1, table, writer)?;
            write!(writer, " ")?;
            name_of_struct(*i.1, table, writer)?;
            write!(writer, ";")?;
        }
        for i in &self.str2func {
            if self.funcs[*i.1].import {
                continue;
            }
            self.funcs[*i.1].ret.gen_c_code(table, writer)?;
            write!(writer, " ")?;
            name_of_func(*i.1, table, writer)?;
            write!(writer, "(")?;
            let mut it = self.funcs[*i.1].args.iter().peekable();
            while let Some(val) = it.next() {
                val.1.gen_c_code(table, writer)?;
                if let Some(_) = it.peek() {
                    write!(writer, ",")?;
                }
            }
            write!(writer, ");")?;
        }
        for i in &self.str2struct {
            write!(writer, "struct ")?;
            name_of_struct(*i.1, table, writer)?;
            write!(writer, "{{")?;
            for arg in self.structs[*i.1].elements.iter() {
                arg.1.gen_c_code(table, writer)?;
                write!(writer, " {name};", name = arg.0)?;
            }
            write! {writer,"}};"}?;
        }
        for i in &self.str2func {
            if self.funcs[*i.1].import {
                continue;
            }
            self.funcs[*i.1].ret.gen_c_code(table, writer)?;
            write!(writer, " ")?;
            name_of_func(*i.1, table, writer)?;
            write!(writer, "(")?;
            let mut it = self.funcs[*i.1].args.iter().peekable();
            let mut namer = 0;
            while let Some(val) = it.next() {
                val.1.gen_c_code(table, writer)?;
                write!(writer, " v{namer}")?;
                namer += 1;
                if let Some(_) = it.peek() {
                    write!(writer, ",")?;
                }
            }
            write!(writer, ")")?;
            self.funcs[*i.1].code.gen_c_code(table, writer)?;
        }
        Ok(())
    }
}

impl CGen for ZagapType {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        match self {
            Self::Ptr(ptr) => {
                write!(writer, "")?;
                (*ptr).gen_c_code(table, writer)?;
                write!(writer, "*")?;
            }
            Self::Struct(id) => name_of_struct(*id, table, writer)?,
            Self::Array { t, size } => {
                t.gen_c_code(table, writer)?;
                write!(writer, "[{size}])")?;
            }
            Self::Inbuilt(t) => t.gen_c_code(table, writer)?,
        }
        Ok(())
    }
}

impl CGen for InbuiltType {
    fn gen_c_code<T: io::Write>(&self, _table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        write!(
            writer,
            "{name}",
            name = match self {
                Self::Void => "void",
                Self::Bool => "_Bool",
                Self::I8 => "int8_t",
                Self::I16 => "int16_t",
                Self::I32 => "int32_t",
                Self::I64 => "int32_t",
                Self::U8 => "uint8_t",
                Self::U16 => "uint16_t",
                Self::U32 => "uint32_t",
                Self::U64 => "uint64_t",
                Self::USIZE => "uintptr_t",
                Self::ISIZE => "intptr_t",
                Self::C8 => "char",
                Self::C16 => "int16_t",
                Self::C32 => "int32_t",
                Self::F32 => "float",
                Self::F64 => "double",
                Self::F128 => "(long double)",
            }
        )?;
        Ok(())
    }
}

fn name_of_func<T: io::Write>(id: usize, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
    if let Some(name) = table.funcs[id].export_name {
        write!(writer, "{name}")?;
    } else {
        write!(writer, "f{id}")?;
    }
    Ok(())
}

fn name_of_struct<T: io::Write>(id: usize, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
    if let Some(name) = table.structs[id].export_name {
        write!(writer, "{name}")?;
    } else {
        write!(writer, "s{id}")?;
    }
    Ok(())
}

impl CGen for CodeBlock<'_> {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        write!(writer, "{{")?;
        for i in self.vars.iter() {
            i.1 .1.gen_c_code(table, writer)?;
            write!(writer, " v{name};", name = i.1 .0)?; //TODO: init 0;
        }
        for i in self.statements.iter() {
            i.gen_c_code(table, writer)?;
        }
        write!(writer, "}}")?;
        Ok(())
    }
}

impl CGen for Statement<'_> {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        match self {
            Self::None => {}
            Self::Assigment(lhs, rhs) => {
                lhs.gen_c_code(table, writer)?;
                write!(writer, "=")?;
                rhs.gen_c_code(table, writer)?;
                write!(writer, ";")?;
            }
            Self::Expr(expr) => {
                expr.gen_c_code(table, writer)?;
                write!(writer, ";")?;
            }
            Self::Code(c) => c.gen_c_code(table, writer)?,
            Self::IfBlock {
                cond,
                ftrue,
                ffalse,
            } => {
                write!(writer, "if(")?;
                cond.gen_c_code(table, writer)?;
                write!(writer, ")")?;
                ftrue.gen_c_code(table, writer)?;
                if let Some(other) = ffalse {
                    write!(writer, "else")?;
                    other.gen_c_code(table, writer)?;
                }
            }
            Self::Cfor {
                init,
                cond,
                post,
                code,
            } => todo!(),
            Self::Cwhile(expr, code) => {
                write!(writer, "while(")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ")")?;
                code.gen_c_code(table, writer)?;
            }
            Self::Ret(expr) => {
                write!(writer, "return ")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ";")?;
            }
            Self::Cti => write!(writer, "continue;")?,
            Self::Brk => write!(writer, "break;")?,
            _ => todo!(),
        }
        Ok(())
    }
}

impl CGen for Expr<'_> {
    fn gen_c_code<T: io::Write>(&self, table: &ProgramTable, writer: &mut T) -> io::Result<()> {
        match self {
            Self::Etypechange(expr, t) => {
                write!(writer, "((")?;
                t.gen_c_code(table, writer)?;
                write!(writer, ")")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ")")?;
            }
            Self::EUnary { op, val } => {
                write!(
                    writer,
                    "{p}(",
                    p = match op {
                        UnaryOp::Minus => "-",
                        UnaryOp::Lnot => "!",
                        UnaryOp::Bnot => "~",
                    }
                )?;
                val.gen_c_code(table, writer)?;
                write!(writer, ")")?;
            }
            Self::EBinary { lhs, rhs, op } => {
                write!(writer, "(")?;
                lhs.gen_c_code(table, writer)?;
                write!(
                    writer,
                    "){p}(",
                    p = match op {
                        BinaryOp::Plus => "+",
                        BinaryOp::Minus => "-",
                        BinaryOp::Asterix => "*",
                        BinaryOp::Div => "/",
                        BinaryOp::Modulo => "%",
                        BinaryOp::Leq => "==",
                        BinaryOp::Lless => "<",
                        BinaryOp::Llesseq => "<=",
                        BinaryOp::Lmore => ">",
                        BinaryOp::Lmoreeq => ">=",
                        BinaryOp::Lneq => "!=",
                        BinaryOp::Land => "&&",
                        BinaryOp::Lor => "||",
                        BinaryOp::Band => "&",
                        BinaryOp::Bor => "|",
                        BinaryOp::Bxor => "^",
                        BinaryOp::Bshiftr => ">>",
                        BinaryOp::Bshiftl => "<<",
                    }
                )?;
                rhs.gen_c_code(table, writer)?;
                write!(writer, ")")?;
            }
            Self::EDref(expr) => {
                write!(writer, "*(")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ")")?;
            }
            Self::EPtr(expr) => {
                write!(writer, "&(")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ")")?;
            }
            Self::EFuncCall(id, args) => {
                name_of_func(*id, table, writer)?;
                write!(writer, "(")?;
                let mut it = args.iter().peekable();
                while let Some(arg) = it.next() {
                    arg.gen_c_code(table, writer)?;
                    if let Some(_) = it.peek() {
                        write!(writer, ",")?;
                    }
                }
                write!(writer, ")")?;
            }
            Self::EClit(_c) => todo!(),
            Self::Eslit(s) => write!(writer, "{s}")?,
            Self::Enlit(n) => write!(writer, "{n}")?,
            Self::EVar(v) => write!(writer, "v{v}")?,
            Self::EElement(expr, name) => {
                write!(writer, "(")?;
                expr.gen_c_code(table, writer)?;
                write!(writer, ").{name}")?;
            }
            Self::EIndex { lhs, rhs } => {
                lhs.gen_c_code(table, writer)?;
                write!(writer, "[")?;
                rhs.gen_c_code(table, writer)?;
                write!(writer, "[")?;
            }
        }
        Ok(())
    }
}
