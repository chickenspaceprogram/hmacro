use crate::lexer::{Token, pop_tok, Location, TokenVal};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Copy, Debug, Clone)]
pub enum Text<'a> {
    Ref(&'a str),
    Chr(u8),
    Empty,
    Arg(&'a str),
}

impl <'a>Text<'a> {
    pub fn from_tok(tok: &TokenVal<'a>) -> Option<Self> {
        match tok {
            TokenVal::Text(txt) => Some(Text::Ref(txt)),
            TokenVal::EscChr(b'\n') => Some(Text::Empty),
            TokenVal::EscChr(ch) => Some(Text::Chr(*ch)),
            TokenVal::Arg(st) => Some(Text::Arg(st)),
            _ => None,
        }
    }
}

enum Arg<'a> {
    Text(&'a str),

    Macro(&'a str),
}

pub struct Macro<'a> {
    name: &'a str,
    args: Vec<Arg<'a>>,
    bracket_ctr: usize,
}

pub enum ParserTok<'a> {
    Text(Text<'a>),
    Macro(Macro<'a>),
    Error(&'static str),
}

pub struct ErrType {
    what: &'static str,
    loc: Location,
}

fn get_matching_bracket

pub fn expand_macros<'a>(
    txt: &mut &'a str, 
    macros: &mut HashMap<&'a str, Vec<Token<'a>>>, 
    filepath: &Path,
    loc: &mut Location,
) -> Result<Vec<Text<'a>>, Vec<ErrType>> {
    let mut nbegins = 0;
    let mut outvec = Vec::new();
    let mut errvec = Vec::new();
    let mut in_macro: bool = false;

    while let Some(tok) = pop_tok(txt, loc) {
        match tok.tok {
            TokenVal::Error(emsg) => errvec.push(ErrType {what: emsg, loc: *loc}),
            _ => outvec.push(Text::from_tok(&tok.tok).unwrap()),

        }
    }

    if errvec.len() > 0 {
        return Err(errvec);
    }
    return Ok(outvec);
}

