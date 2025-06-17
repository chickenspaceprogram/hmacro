use std::env;
use std::path::PathBuf;
use std::process::ExitCode;
mod lexer;
mod parser;
pub mod expand;

fn print_err(e: &expand::ErrType) {
    let (row, col, msg, path) = e;
    eprintln!("\x1b[1m{}:{}:{}:\x1b[31m error:\x1b[22;39m {}.", path.display(), row, col, msg);
}

fn print_version() {
    println!("
    hmacro v2.1.1, Copyright (C) 2025 Athena Boose

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

fn main() -> ExitCode {
    let mut args = env::args().peekable();
    args.next();
    match args.peek() {
        Some(arg) => {
            if arg == "--help" || arg == "-h" {
                print_help();
                return ExitCode::SUCCESS;
            }
            if arg == "--version" || arg == "-v" {
                print_version();
                return ExitCode::SUCCESS;
            }
            if arg == "--license" {
                print_license();
                return ExitCode::SUCCESS;
            }
        },
        None => {
            print_help();
            return ExitCode::SUCCESS;
        },
    }
    for arg in args {
        match expand::expand_new_file(PathBuf::from(arg).as_path()) {
            Ok(txt) => print!("{}", txt),
            Err(e) => {
                print_err(&e);
                return ExitCode::FAILURE;
            },
        }
    }
    return ExitCode::SUCCESS;
}
