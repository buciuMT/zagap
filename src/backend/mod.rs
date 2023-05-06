use std::io;

use crate::ast::ProgramTable;

use self::cbackend::CGen;
mod cbackend;

pub fn generate_c<T: io::Write + io::Seek>(
    table: &ProgramTable,
    writer: &mut T,
    headers: &Option<Vec<String>>,
) -> io::Result<()> {
    if let Some(header) = headers {
        for i in header.iter() {
            write!(writer, "#include<{i}>\n")?;
        }
    } else {
        write!(
            writer,
            "#include<stdint.h>\n#include<stdlib.h>\n#include<stdio.h>\n"
        )?;
    }
    table.gen_c_code(table, writer)?;
    Ok(())
}
