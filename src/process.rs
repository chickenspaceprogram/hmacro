use std::{fs, path::{Path, PathBuf}, collections::HashMap};
mod lexer;
mod parser;
mod expander;

#[derive(Clone, Debug, PartialEq)]
pub enum ExpandedElem<'a> {
    Text(&'a str),
    EscChr(char),
    ArgNo(u64),
}

pub type Text<'a> = Vec<ExpandedElem<'a>>;

impl<'a> ExpandedElem<'a> {
    fn to_string(&self) -> String {
        match self {
            ExpandedElem::Text(txt) => txt.to_string(),
            ExpandedElem::EscChr(ch) => ch.to_string(),
            ExpandedElem::ArgNo(n) => n.to_string(),
        }
    }
}
pub type MacroMap<'a> = HashMap<String, Text<'a>>;

fn reduce_text<'a>(txt: &Text<'a>) -> String {
    txt.iter().fold(String::new(), |acc: String, elem| str_append(acc, elem.to_string()))
}

pub fn process_file(path: &Path, macros: &mut MacroMap) -> Result<String, Vec<String>> {
    let txt = match fs::read_to_string(path) {
        Ok(txt) => txt,
        Err(e) => return Err(Vec::from([e.to_string()])),
    };
    match expander::expand(parser::parse(lexer::tokenize(txt.as_str()).as_ref()).as_ref(), macros, path) {
        Ok(v) => Ok(reduce_text(&v)),
        Err(es) => Err(
            es.iter()
              .map(|(r, c, msg)| 
                "Row ".to_string() + r.to_string().as_str() + 
                ", Col " + c.to_string().as_str() + 
                ": " + msg)
              .collect()
        ),
    }
}

fn str_append(mut a: String, b: String) -> String {
    a.push_str(b.as_str());
    a
}
