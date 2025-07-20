use std::env;
use std::process::ExitCode;
use std::fs;
use std::collections::HashMap;
mod stackbuf;
mod parser;


fn print_version() {
    println!("
    hmacro v3.0.0, Copyright (C) 2025 Athena Boose

    This program comes with ABSOLUTELY NO WARRANTY.
    This is free software, and you are welcome to redistribute it
    under certain conditions; see the GNU General Public License for details.
    ")
}

fn print_help() {
    println!("
    Usage:

    hmacro [OPTIONS] <filename>

    `-v', `--version' - Print a version message, then exit
    `-h', `--help'    - Print a help message, then exit
    `--license'       - Display information about hmacro's license
    `-Dmacro=exp'     - Predefines a new macro, named `macro', that expands
                        to `exp'.

    You can pass a list of filenames to hmacro.
    They will each be expanded separately and the results concatenated.
    ");
}

fn print_license() {
    println!("
    Copyright (C) 2025 Athena Boose

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
    ");
}

struct CompileInfo {
    files: Vec<String>,
    defines: Vec<(String, Vec<parser::MacroDefElem>)>
}

impl CompileInfo {
    fn new() -> Self {
        CompileInfo {
            files: Vec::new(),
            defines: Vec::new(),
        }
    }
}

enum ArgsInfo {
    Compile(CompileInfo),
    Help,
    Version,
    License,
}

fn get_args_info() -> Result<ArgsInfo, Vec<String>> {
    let mut it = env::args();
    it.next();
    if let Some(_) = it.find(|item| item == "--help" || item == "-h") {
        return Ok(ArgsInfo::Help);
    }
    let mut it = env::args();
    it.next();
    if let Some(_) = it.find(|item| item == "--version" || item == "-v") {
        return Ok(ArgsInfo::Version);
    }
    let mut it = env::args();
    it.next();
    if let Some(_) = it.find(|item| item == "--license") {
        return Ok(ArgsInfo::License);
    }
    let mut it = env::args();
    it.next();
    let mut compinfo = CompileInfo::new();
    for arg in it {
        let mut iter = arg.chars();
        match (iter.next(), iter.next()) {
            (Some('-'), Some('D')) => compinfo.defines.push(parse_define(&arg[2..])?),
            _ => compinfo.files.push(arg),
        }
    }
    return Ok(ArgsInfo::Compile(compinfo));
}

fn parse_define(arg: &str) -> Result<(String, Vec<parser::MacroDefElem>), Vec<String>> {
    if let Some((nm, exp)) = arg.split_once('=') {
        if parser::is_macro_name(nm) {
            return Ok((nm.to_string(), parser::parse_def_arg(exp)?));
        }
    }
    else if parser::is_macro_name(arg) {
        return Ok((arg.to_string(), Vec::new()));
    }
    let mut e = Vec::new();
    e.push("Invalid macro name in macro predefinition".to_string());
    return Err(e);
}

fn main() -> ExitCode {
    match get_args_info() {
        Ok(ArgsInfo::Help) => {
            print_help();
            return ExitCode::SUCCESS;
        },
        Ok(ArgsInfo::Version) => {
            print_version();
            return ExitCode::SUCCESS;
        },
        Ok(ArgsInfo::License) => {
            print_license();
            return ExitCode::SUCCESS;
        },
        Ok(ArgsInfo::Compile(comp)) => {
            let mut macmap = HashMap::new();
            for (nm, exp) in &comp.defines {
                macmap.insert(nm.clone(), exp.clone());
            }
            for elem in &comp.files {
                match fs::read_to_string(elem) {
                    Ok(filtxt) => {
                        let mut mmap = macmap.clone();
                        match parser::parse(filtxt.as_str(), &mut mmap) {
                            Ok(txt) => print!("{}", txt),
                            Err(es) => parser::eprint_errors(es.as_slice(), elem.as_str()),
                        }
                    },
                    Err(e) => {
                        eprintln!("Fatal error while reading file: {}", e);
                        return ExitCode::FAILURE;
                    }
                }
            }
            return ExitCode::SUCCESS;
        }
        Err(e) => {
            eprintln!("Fatal errors while parsing command arguments:");
            for elem in e {
                eprintln!("{}", elem);
            }
            return ExitCode::FAILURE;
        },
    }
}
