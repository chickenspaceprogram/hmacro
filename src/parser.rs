use crate::lexer::{Token, tokenize};

#[derive(Debug, Clone, PartialEq)]
enum TokenTree<'a> {
    Text(usize, usize, &'a str),
    EscChr(usize, usize, char),
    ArgNo(usize, usize, usize),
    Macro(usize, usize, &'a str, Vec<TokenTree<'a>>), // tokentrees are probably scopes but don't inherently have to be
    Scope(usize, usize, Vec<TokenTree<'a>>),
    Error(usize, usize, &'static str),
    Nothing,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroAst {
    Text(usize, usize, String),
    EscChr(usize, usize, char),
    ArgNo(usize, usize, usize),
    Macro(usize, usize, String, Vec<MacroAst>), // tokentrees are probably scopes but don't inherently have to be
    Scope(usize, usize, Vec<MacroAst>),
    Error(usize, usize, &'static str),
}

pub fn parse_txt(txt: &str) -> Vec<MacroAst> {
    parse(tokenize(txt).as_ref()).iter().map(|tr| convert(tr)).collect()
}

fn convert(toktree: &TokenTree) -> MacroAst {
    match toktree {
        TokenTree::Text(r, c, txt) => MacroAst::Text(*r, *c, txt.to_string()),
        TokenTree::EscChr(r, c, ch) => MacroAst::EscChr(*r, *c, *ch),
        TokenTree::ArgNo(r, c, v) => MacroAst::ArgNo(*r, *c, *v),
        TokenTree::Macro(r, c, nm, child) => MacroAst::Macro(*r, *c, nm.to_string(), child.iter().map(|el| convert(el)).collect()),
        TokenTree::Scope(r, c, child) => MacroAst::Scope(*r, *c, child.iter().map(|el| convert(el)).collect()),
        TokenTree::Error(r, c, e) => MacroAst::Error(*r, *c, e),
        _ => panic!("found TokenTree::Nothing in parse tree"),
    }
}

fn parse<'a, 'b>(toks: &'b [(usize, usize, Token<'a>)]) -> Vec<TokenTree<'a>> {
    let mut tokslice = toks;
    loop {
        let (mut tvec, rest) = parse_internal(tokslice);
        if rest.len() == 0 {
            return tvec;
        }
        match rest[0] {
            (r, c, Token::RBrack) => {
                tvec.push(TokenTree::Error(r, c, "Extra right bracket"));
                tokslice = &rest[1..];
            },
            (r, c, _) => {
                tvec.push(TokenTree::Error(r, c, "Parse error"));
                tokslice = rest;
            },
        }
    }
}

fn parse_scope<'a, 'b>(toks: &'b [(usize, usize, Token<'a>)]) -> Option<(TokenTree<'a>, &'b [(usize, usize, Token<'a>)])> {
    if toks.len() == 0 {
        return None;
    }
    if let (row, col, Token::LBrack) = toks[0] {
        let (tok, rest) = parse_internal(&toks[1..]);
        if tok.len() == 0 {
            return Some((TokenTree::Error(row, col, "Open bracket followed by end of file"), rest));
        }
        if rest.len() == 0 {
            return Some((TokenTree::Error(row, col, "Failed to find closing bracket"), &toks[1..]))
        }
        return match rest[0] {
            (_, _, Token::RBrack) => Some((TokenTree::Scope(row, col, tok), &rest[1..])),
            (r, c, _) => Some((TokenTree::Error(r, c, "Parse error"), rest)),
        };
    }
    return None;
}

fn grab_tok<'a, 'b>(toks: &'b [(usize, usize, Token<'a>)]) -> (TokenTree<'a>, &'b [(usize, usize, Token<'a>)]) {
    let mut tokslice = toks;
    if toks.len() == 0 {
        return (TokenTree::Nothing, toks);
    }
    match toks[0] {
        (row, col, Token::Text(s)) => (TokenTree::Text(row, col, s), &tokslice[1..]),
        (row, col, Token::ArgNo(v)) => (TokenTree::ArgNo(row, col, v), &tokslice[1..]),
        (row, col, Token::EscChr(ch)) => (TokenTree::EscChr(row, col, ch), &tokslice[1..]),
        (_, _, Token::LBrack) => parse_scope(tokslice).unwrap(),
        (_, _, Token::RBrack) => (TokenTree::Nothing, tokslice),
        (row, col, Token::MacroName(nm)) => {
            tokslice = &tokslice[1..];
            let mut tokvec: Vec<TokenTree<'a>> = Vec::new();
            while let Some((tok, rest)) = parse_scope(tokslice) {
                tokvec.push(tok);
                tokslice = rest;
            }
            (TokenTree::Macro(row, col, nm, tokvec), tokslice)
        },
    }
}
fn parse_internal<'a, 'b>(toks: &'b [(usize, usize, Token<'a>)]) -> (Vec<TokenTree<'a>>, &'b [(usize, usize, Token<'a>)]) {
    let mut tokslice = toks;
    let mut tokvec: Vec<TokenTree<'a>> = Vec::new();
    loop {
        match grab_tok(tokslice) {
            (TokenTree::Nothing, _) => break,
            (tok, rest) => {
                tokvec.push(tok);
                tokslice = rest;
            },
        }
    }
    return (tokvec, tokslice);
}

