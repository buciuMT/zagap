mod ast;
mod backend;
mod importer;
mod parser;
use backend::generate_c;
use importer::get_all_imports;
use parser::parse_programtable;
use std::fs::{self, create_dir_all, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::{env, process};

enum CompileMode {
    Genc,
    Obj,
    Build,
    Run,
}

fn help<T>() -> T {
    println!("Usage: zagap [mode] [arguments]");
    exit(0);
}

fn main() -> std::io::Result<()> {
    let dirs = directories::ProjectDirs::from("org", "user", "zagap").unwrap();
    let stdlib_dir = dirs.data_dir().join("stdlib");
    if !stdlib_dir.exists() {
        create_dir_all(&stdlib_dir)?;
    }
    if !dirs.cache_dir().exists() {
        create_dir_all(dirs.cache_dir())?;
    }

    let mut sargs = env::args().peekable();
    let _current_executable_path = sargs.next().unwrap_or_else(help);
    let compile_mode = match sargs.peek().unwrap_or_else(help).as_str() {
        "genc" => CompileMode::Genc,
        "obj" => CompileMode::Obj,
        "build" => CompileMode::Build,
        "run" => CompileMode::Run,
        "addlib" => {
            sargs.next();
            for i in sargs {
                let path = Path::new(&i);
                fs::copy(path, stdlib_dir.join(path.file_name().unwrap()))?;
            }
            process::exit(0);
        }
        _ => panic!("Unknown mode"),
    };
    let args = arguments::parse(sargs).expect("Argument parse has failed");
    let cc = args
        .get::<String>("cc")
        .unwrap_or_else(|| "tcc".to_string());

    //libs
    let libfolders = args.get_all::<String>("lib").unwrap_or_else(|| Vec::new());
    let mut libfolders: Vec<&Path> = libfolders.iter().map(|x| Path::new(x)).collect();
    libfolders.push(stdlib_dir.as_path());

    let extra = args
        .get_all::<String>("extra")
        .unwrap_or_else(|| Vec::new());
    let headers = args
        .get_all::<String>("header")
        .unwrap_or_else(|| Vec::new());
    let main_file = args
        .get::<String>("f")
        .unwrap_or_else(|| "main.zagap".to_string());
    let out = args
        .get::<String>("o")
        .unwrap_or_else(|| match compile_mode {
            CompileMode::Genc => format!("{main_file}.c"),
            CompileMode::Obj => format!("{main_file}.o"),
            CompileMode::Build => "exe".to_string(),
            CompileMode::Run => "exe".to_string(),
        });
    let paths = get_all_imports(Path::new(&main_file), &libfolders);
    let code: String = paths
        .iter()
        .map(|x| fs::read_to_string(x))
        .flatten()
        .collect();

    let ast = parse_programtable(code.as_str());

    let cfile_path = match compile_mode {
        CompileMode::Genc => PathBuf::from(&out),
        _ => dirs.cache_dir().join("tmp.c"),
    };
    let mut buffwriter = BufWriter::new(File::create(&cfile_path)?);
    let header = if headers.len() == 0 {
        None
    } else {
        Some(headers)
    };

    generate_c(&ast, &mut buffwriter, &header)?;
    buffwriter.flush()?;
    let cmd_log_path = dirs.cache_dir().join("log");
    let cmd_log = File::create(cmd_log_path).expect("Unable to create log file");
    match compile_mode {
        CompileMode::Genc => {}
        CompileMode::Build => {
            Command::new(cc.to_string())
                .stderr(cmd_log)
                .arg(&cfile_path)
                .args(extra)
                .arg(format!("-o{out}"))
                .spawn()?
                .wait()?;
        }
        CompileMode::Obj => {
            Command::new(cc.to_string())
                .stderr(cmd_log)
                .arg(&cfile_path)
                .args(extra)
                .arg("-c")
                .arg(format!("-o{out}"))
                .spawn()?
                .wait()?;
        }
        CompileMode::Run => {
            if Command::new(cc.to_string())
                .stderr(cmd_log)
                .arg(&cfile_path)
                .args(extra)
                .arg(format!("-o{out}"))
                .spawn()?
                .wait()?
                .success()
            {
                Command::new(format!("./{out}")).spawn()?.wait()?;
            }
        }
    }

    Ok(())
}
