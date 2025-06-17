use crate::process::{ExpandedElem, reduce_text, MacroMap, Text, parser::TokenTree};
use std::{path::{Path, PathBuf}, collections::HashMap};

type ErrType = (usize, usize, &'static str);

type InbuiltMacro<'a, 'b> = (String, fn(args: &'b [Text<'a>], macro_map: &mut MacroMap, current_file: &Path) -> Result<&'b [String], &'static str>);

fn is_text<'a>(el: &ExpandedElem<'a>) -> bool {
    if let ExpandedElem::Text(_) = el {
        return true;
    }
    return false;
}

fn get_text<'a>(mut txt: String, el: &ExpandedElem<'a>) -> String {
    if let ExpandedElem::Text(eltxt) = el {
        txt.push_str(eltxt);
        return txt;
    }
    else {
        panic!("An internal error occurred. Failed to filter out bad macro arg elements.");
    }
}

fn combine_paths(root_file: &Path, rel_path: &Path) -> PathBuf {
    let mut buf = root_file.to_path_buf();
    buf.pop();
    buf.join(rel_path)
}


fn expand_def<'a, 'b>(args: &'b [Text<'a>], macro_map: &mut HashMap<String, Text<'a>>) -> Result<&'b [String], &'static str> {
    if args.len() != 2 {
        return Err("Macro \\def must have exactly 2 arguments");
    }
    let macroname = reduce_text(&args[0]);
    if let Some(_) = macroname.find(|ch: char| !(ch.is_alphanumeric() || ch == '-' || ch == '_')) {
        return Err("Attempted to define an invalid macro name");
    }
    macro_map.insert(macroname, args[1].clone());
    return Ok(&args[0..1]);
}

fn expand_include<'a, 'b>(args: &'b [Text<'a>], macro_map: &mut HashMap<String, Text<'a>>) -> Result<&'b [Text<'a>], &'static str> {
    if args.len() != 1 {
        return Err("Macro \\include must have exactly 1 argument");
    }
    reduce_text(args[0])
}


pub fn expand<'a, 'b>(txt: &'b [TokenTree<'a>], macros: &mut MacroMap, path: &Path) -> Result<Text<'a>, Vec<ErrType>> {
    todo!();
}
