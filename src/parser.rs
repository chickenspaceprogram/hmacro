use crate::lexer::{Token, pop_tok, Location, TokenVal};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Copy, Clone, Debug)]
enum TextElem<'a> {
    Text(&'a str),
    Chr(u8),
    Arg(usize),
}

type MacroExpansion<'a> = Vec<TextElem<'a>>;


fn parse_once<'a>(
    instack: &mut Vec<TextElem<'a>>,
    macro_map: &mut HashMap<String, MacroExpansion<'a>>,
    base_path: &Path,
    loc: &mut Location,
) -> Option<Vec<TextElem<'a>>> {
    match instack.pop()? {
        TextElem::Text(mut txt) => {
            loop {
                let tok = pop_tok(&mut txt, loc);
            }
        },
        val => {
            let mut outvec = Vec::new();
            outvec.push(val);
            return Some(outvec);
        },
    }
}


