// hmacro - a macro preprocessor
// Copyright (C) 2025 Athena Boose 

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

// SPDX-License-Identifier: GPL-3.0-or-later

use std::env;
use std::path::PathBuf;
use std::process::ExitCode;
use std::collections::HashMap;
mod lexer;
mod parser;
pub mod expand;

fn print_err(e: &expand::ErrType) {
    let (row, col, msg, path) = e;
    eprintln!("\x1b[1m{}:{}:{}:\x1b[31m error:\x1b[22;39m {}.", path.display(), row, col, msg);
}

fn print_version() {
    println!("
    hmacro v2.2.2, Copyright (C) 2025 Athena Boose

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

enum ArgsInfo {
    Compile(Vec<String>, HashMap<String, Vec<parser::MacroAst>>), // fnames, defines
    Help,
    Version,
    License,
}

fn get_args_info() -> Result<ArgsInfo, &'static str> {
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
    let macros: HashMap<String, Vec<parser::MacroAst>> = 
        it.filter(|el| el.chars().count() >= 2 && el.chars().nth(0).unwrap() == '-' && el.chars().nth(1).unwrap() == 'D')
          .map(|el| parse_define(el.as_str()))
          .collect::<Result<Vec<(String, String)>, _>>()?
          .iter()
          .map(|(nm, exp)| (nm.to_string(), Vec::from([parser::MacroAst::Text(0, 0, exp.to_string())])))
          .collect();
    let mut it = env::args();
    it.next();
    let files: Vec<String> = 
        it.filter(|el| el.chars().count() < 2 || el.chars().nth(0).unwrap() != '-' || el.chars().nth(1).unwrap() != 'D')
          .collect();
    return Ok(ArgsInfo::Compile(files, macros));
}

fn parse_define(arg: &str) -> Result<(String, String), &'static str> {
    let v: Vec<&str> = arg[2..].splitn(2, '=').collect();
    if v.len() < 2 {
        return Err("Invalid macro predefinition");
    }
    if v[0].contains(|ch: char| !(ch.is_alphanumeric() || ch == '-' || ch == '_')) {
        return Err("Predefined macro name contains invalid characters");
    }
    return Ok((v[0].to_string(), v[1].to_string()));
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
        Ok(ArgsInfo::Compile(files, macros)) => {
            for file in files {
                match expand::expand_file(PathBuf::from(file).as_path(), &mut macros.clone()) {
                    Ok(txt) => print!("{}", txt),
                    Err(e) => {
                        print_err(&e);
                        return ExitCode::FAILURE;
                    },
                }
            }
            return ExitCode::SUCCESS;
        },
        Err(e) => {
            println!("Fatal error while parsing CLI args: {}", e);
            return ExitCode::FAILURE;
        },
    }
}
