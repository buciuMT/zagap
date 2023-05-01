use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[grammar = "importer/importer.pest"] // relative to project `src`
struct ImportParser;

pub fn get_all_imports(file: &Path, lib_folders: Vec<&Path>) -> Vec<PathBuf> {
    let mut res = vec![file.to_path_buf()];
    let mut imported = BTreeSet::<String>::new();
    let mut to_be_imported = Vec::<String>::new();
    let mut code = fs::read_to_string(&file).expect("Unable to read the given file");
    let folder = file.parent().expect("Unable to find the root folder");
    loop {
        let ast = ImportParser::parse(Rule::all, code.as_str())
            .expect("Unable to parse imports")
            .next()
            .unwrap();
        parse_all(ast, &mut to_be_imported);
        while to_be_imported.len() != 0 && imported.contains(to_be_imported.last().unwrap()) {
            to_be_imported.pop();
        }
        if to_be_imported.is_empty() {
            break;
        }
        let last = to_be_imported.last().unwrap();
        let file = folder.join(last).with_extension("zagap");
        if file.exists() && file.is_file() {
            code = fs::read_to_string(&file).expect("unable to read imports");
            res.push(file);
        } else if !'cond: {
            for libf in &lib_folders {
                let lib = libf.join(last).with_extension("zagap");
                if lib.exists() {
                    code = fs::read_to_string(&file).expect("unable to read imports");
                    res.push(lib);
                    break 'cond true;
                }
            }
            false
        } {
            eprintln!("Unable to find file for import `{last}`");
            panic!();
        }
        imported.insert(to_be_imported.pop().unwrap());
    }
    res
}

fn parse_all(ast: Pair<Rule>, to_be_imported: &mut Vec<String>) {
    for i in ast.into_inner() {
        match i.as_rule() {
            Rule::import => {
                to_be_imported.push(i.into_inner().as_str().into());
            }
            Rule::other => {
                break;
            }
            _ => unreachable!(),
        }
    }
}

#[test]
fn empty_import() {
    get_all_imports(
        Path::new("./src/tests/imports/import_parser/empty_import/main.zagap"),
        Vec::new()
    );
}
#[test]
fn recursive_import() {
    assert_eq!(
        get_all_imports(
            Path::new("./src/tests/imports/import_parser/recursive_import/main.zagap"),
            Vec::new(),
        ),
        vec![
            Path::new("./src/tests/imports/import_parser/recursive_import/main.zagap"),
            Path::new("./src/tests/imports/import_parser/recursive_import/a.zagap"),
            Path::new("./src/tests/imports/import_parser/recursive_import/b.zagap")
        ]
    );
}
