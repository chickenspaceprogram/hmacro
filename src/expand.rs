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

use crate::parser::{parse_txt, MacroAst};
use std::{fs, path::PathBuf, sync::LazyLock, collections::HashMap};
pub use std::path::Path;

pub type ErrType = (usize, usize, String, PathBuf);
type InbuiltMacroFn = fn (args: MacroInfo, macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType>;
type MacroInfo<'a> = (usize, usize, &'a [MacroAst]);

static INBUILT_MACROS: LazyLock<HashMap<&str, InbuiltMacroFn>> = LazyLock::new(|| {
    let mut map: HashMap<&str, InbuiltMacroFn> = HashMap::new();
    map.insert("def", def_macro);
    map.insert("include", include_macro);
    map.insert("incldefs", incldefs_macro);
    map.insert("cat", cat_macro);
    map
});

fn def_macro(info: MacroInfo, macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    let (line, col, args) = info;
    if args.len() < 2 {
        return Err((line, col, "\\def macro requires at least 2 arguments".to_string(), path.to_path_buf()))
    }
    let name = expand(&args[0], macro_map, path)?; // args are scopes so we don't have to clone
    if let Some(_) = name.find(|ch: char| !(ch.is_alphanumeric() || ch == '-' || ch == '_')) {
        return Err((line, col, "Name of newly-defined character is invalid".to_string(), path.to_path_buf()));
    }
    if let MacroAst::Scope(_, _, expansion) = &args[1] {
        macro_map.insert(name, expansion.clone());
    }
    else {
        return Err((line, col, "hmacro internal error: Argument to vector is not a scope".to_string(), path.to_path_buf()));
    }
    Ok(String::new())
}

fn include_macro(info: MacroInfo, macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    let (row, col, args) = info;
    if args.len() < 1 {
        return Err((row, col, "\\include macro requires at least 1 argument".to_string(), path.to_path_buf()))
    }
    let filename = expand(&args[0], macro_map, path)?;
    let mut newpath = path.parent().unwrap_or(path).to_path_buf();
    newpath.push(filename);
    match std::fs::exists(&newpath) {
        Ok(false) => return Err((row, col, 
            format!("no such file or directory `{}\'", 
                newpath
                    .into_os_string()
                    .into_string()
                    .unwrap_or("[filename contains non-unicode characters]".to_string())
                ), path.to_path_buf())),
        Ok(true) => (),
        Err(e) => return Err((row, col, e.to_string(), path.to_path_buf())),
    }
    expand_file(newpath.as_path(), macro_map)
}

fn incldefs_macro(info: MacroInfo, macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    include_macro(info, macro_map, path)?;
    Ok(String::new())
}

fn cat_macro(info: MacroInfo, _: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    let (_, _, ast) = info;
    let mut outstr = String::new();
    for elem in ast {
        match elem {
            MacroAst::Scope(_, _, els) => {
                for i in els {
                    match cat_recursive(i) {
                        Ok(s) => outstr += s.as_str(),
                        Err((r, c, e)) => return Err((r, c, e.to_string(), path.to_path_buf())),

                    }
                }
            },
            _ => {
                match cat_recursive(elem) {
                    Ok(s) => outstr += s.as_str(),
                    Err((r, c, e)) => return Err((r, c, e.to_string(), path.to_path_buf())),
                }
            },
        }
    }
    Ok(outstr)
}

fn cat_recursive(elem: &MacroAst) -> Result<String, (usize, usize, &'static str)> {
    let mut outstr = String::new();
    match elem {
        MacroAst::Text(_, _, txt) => outstr += txt,
        MacroAst::EscChr(_, _, ch) => {
            outstr.push('\\');
            outstr.push(*ch);
        },
        MacroAst::ArgNo(_, _, n) => {
            outstr.push('$');
            outstr.push_str(n.to_string().as_str());
        },
        MacroAst::Macro(_, _, nm, els) => {
            outstr.push('\\');
            outstr += nm;
            for el in els {
                outstr += cat_recursive(el)?.as_str();
            }
        }
        MacroAst::Scope(_, _, scp) => {
            outstr.push('{');
            for el in scp {
                outstr += cat_recursive(el)?.as_str();
            }
            outstr.push('}');
        }
        MacroAst::Error(r, c, e) => return Err((*r, *c, e)),
    }
    Ok(outstr)
}

pub fn expand_new_file(path: &Path) -> Result<String, ErrType> {
    expand_file(path, &mut HashMap::new())
}

pub fn expand_file(path: &Path, macro_map: &mut HashMap<String, Vec<MacroAst>>) -> Result<String, ErrType> {
    match fs::read_to_string(path) {
        Ok(txt) => Ok(concat_all(
            parse_txt(txt.as_str()).iter()
                                   .map(|ast| expand(ast, macro_map, path))
                                   .collect::<Result<Vec<_>,_>>()?
        )),
        Err(e) => Err((0, 0, e.to_string(), path.to_path_buf())),
    }
}
fn expand(ast: &MacroAst, macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    match ast {
        MacroAst::Text(_, _, txt) => Ok(txt.clone()),
        MacroAst::EscChr(_, _, '\n') => Ok(String::new()),
        MacroAst::EscChr(_, _, ch) => Ok(ch.to_string()),
        MacroAst::ArgNo(_, _, n) => Ok(n.to_string()),
        MacroAst::Macro(r, c, name, args) => {
            if let Some(func) = INBUILT_MACROS.get(name.as_str()) {
                return func((*r, *c, args.as_slice()), macro_map, path);
            }
            if let Some(expansion) = macro_map.clone().get(name) { // bad and cringe
                return expand_macro(expansion, args, macro_map, path);
            }
            Err((*r, *c, "Macro `".to_string() + name.as_str() + "\' undefined", path.to_path_buf()))
        },
        MacroAst::Scope(_, _, chld) => {
            chld.iter()
                           .map(|v| expand(v, &mut macro_map.clone(), path)) // changes in subscope happen to clone
                           .collect::<Result<Vec<String>, ErrType>>()
                           .map(|v| concat_all(v))
        },
        MacroAst::Error(r, c, e) => Err((*r, *c, e.to_string(), path.to_path_buf())),
    }
}

fn expand_macro(expansion: &[MacroAst], args: &[MacroAst], macro_map: &mut HashMap<String, Vec<MacroAst>>, path: &Path) -> Result<String, ErrType> {
    let mut outstr = String::new();
    for elem in expansion {
        match elem {
            MacroAst::ArgNo(r, c, n) => {
                if args.len() < *n {
                    return Err((*r, *c, "Macro lacks argument `".to_string() + n.to_string().as_str() + "\'", path.to_path_buf()));
                }
                if *n == 0 {
                    return Err((*r, *c, "Macro cannot have argument $0".to_string(), path.to_path_buf()));
                }
                else {
                    outstr.extend(expand(&args[n - 1], macro_map, path)?.chars());
                }
            },
            any => outstr.extend(expand(any, macro_map, path)?.chars()),
        }
    }
    return Ok(outstr);
}

fn concat_str(mut v1: String, v2: &String) -> String {
    v1.extend(v2.chars());
    v1
}
fn concat_all(strs: Vec<String>) -> String {
    strs.iter().fold(String::new(), |s1, s2| concat_str(s1, s2))
}
