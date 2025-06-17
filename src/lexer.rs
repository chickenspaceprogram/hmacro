#[derive(Debug, Clone)]
pub enum Token<'a> {
    Text(&'a str),
    MacroName(&'a str),
    ArgNo(usize),
    Error(&'static str),
    LBrack,
    RBrack,
    EscChr(char),
}

pub fn tokenize<'a>(txt: &'a str) -> Vec<(usize, usize, Token<'a>)> {
    let mut tok_txt: &'a str = txt;
    let mut out_vec: Vec<Token> = Vec::new();
    loop {
        if tok_txt.len() == 0 {
            break;
        }
        if let Some((tok, rest)) = parse_text(tok_txt) {
            out_vec.push(tok);
            tok_txt = rest;
            continue;
        }
        if let Some((tok, rest)) = parse_macro_name(tok_txt) {
            out_vec.push(tok);
            tok_txt = rest;
            continue;
        }
        if let Some((tok, rest)) = parse_brackets(tok_txt) {
            out_vec.push(tok);
            tok_txt = rest;
            continue;
        }
        if let Some((tok, rest)) = parse_dollar(tok_txt) {
            out_vec.push(tok);
            tok_txt = rest;
            continue;
        }
        if let Some((tok, rest)) = parse_escs(tok_txt) {
            out_vec.push(tok);
            tok_txt = rest;
            continue;
        }
        out_vec.push(Token::Error("Tokenization error: No valid token found."));
        break;
    }
    let mut lineno: usize = 1;
    let mut colno: usize = 1;
    return out_vec.iter().map(|tok| add_linenos(tok, &mut lineno, &mut colno)).collect();
}

fn parse_macro_name<'a>(txt: &'a str) -> Option<(Token<'a>, &'a str)> {
    let mut iter = txt.chars();
    match iter.next() {
        Some('\\') => (),
        _ => return None,
    }
    match iter.next() {
        Some('a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-') => (),
        _ => return None,
    }
    let index = txt[1..].find(|ch: char| !(ch.is_alphanumeric() || ch == '_' || ch == '-')).unwrap_or(txt[1..].len()) + 1;
    return Some((Token::MacroName(&txt[1..index]), &txt[index..]))
}

fn parse_text<'a>(txt: &'a str) -> Option<(Token<'a>, &'a str)> {
    let index = txt.find(|ch| ch == '$' || ch == '\\' || ch == '{' || ch == '}')
                   .unwrap_or(txt.len());
    if index == 0 {
        return None;
    }
    return Some((Token::Text(&txt[..index]), &txt[index..]));
}

fn parse_brackets<'a>(txt: &'a str) -> Option<(Token<'a>, &'a str)> {
    match txt.chars().next() {
        Some('{') => Some((Token::LBrack, &txt[1..])),
        Some('}') => Some((Token::RBrack, &txt[1..])),
        _ => None,
    }
}

fn parse_dollar<'a>(txt: &'a str) -> Option<(Token<'a>, &'a str)> {
    let mut iter = txt.chars();
    match iter.next() {
        Some('$') => (),
        _ => return None,
    }
    let index = txt[1..].find(|ch: char| !ch.is_digit(10)).unwrap_or(txt.len()) + 1;
    match txt[1..index].parse::<usize>() {
        Ok(v) => Some((Token::ArgNo(v), &txt[index..])),
        _ => Some((Token::EscChr('$'), &txt[1..])), 
    }
}

fn parse_escs<'a>(txt: &'a str) -> Option<(Token<'a>, &'a str)> {
    let mut iter = txt.chars();
    match iter.next() {
        Some('\\') => (),
        _ => return None,
    }
    return match iter.next() {
        Some('\\') => Some((Token::EscChr('\\'), &txt[2..])),
        Some('$') => Some((Token::EscChr('$'), &txt[2..])),
        Some('{') => Some((Token::EscChr('{'), &txt[2..])),
        Some('}') => Some((Token::EscChr('}'), &txt[2..])),
        Some('\n') => Some((Token::EscChr('\n'), &txt[2..])),
        _ => None,
    };
}

fn add_linenos<'a>(tok: &Token<'a>, lineno: &mut usize, colno: &mut usize) -> (usize, usize, Token<'a>) {
    let res = match tok {
        Token::Text(s) => (
            countlines(s, *lineno),
            countcols(s, *colno),
            (*lineno, *colno, tok.clone()) // inexpensive as token stores a bunch of references
        ),
        Token::MacroName(s) => (
            countlines(s, *lineno),
            1 + countcols(s, *colno),
            (*lineno, *colno, tok.clone())
        ),
        Token::ArgNo(v) => (
            *lineno,
            *colno + 1 + numdigits(*v),
            (*lineno, *colno, tok.clone())
        ),
        Token::LBrack | Token::RBrack => (
            *lineno,
            *colno + 1,
            (*lineno, *colno, tok.clone())
        ),
        Token::EscChr('\n') => (
            *lineno + 1,
            1,
            (*lineno, *colno, tok.clone())
        ),
        Token::EscChr(_) => (
            *lineno,
            *colno + 2,
            (*lineno, *colno, tok.clone())
        ),
        Token::Error(_) => (
            *lineno,
            *colno,
            (*lineno, *colno, tok.clone())
        ),
    };
    match res {
        (ln, col, tuple) => {
            *lineno = ln;
            *colno = col;
            return tuple;
        }
    }
}

fn numdigits(num: usize) -> usize {
    num.to_string().chars().count()
}

fn countlines(txt: &str, lineno: usize) -> usize {
    lineno + txt.chars().filter(|c| *c == '\n').count()
}

fn countcols(txt: &str, colno: usize) -> usize {
    match txt.lines().enumerate().last() {
        Some((0, s)) => s.len() + colno,
        Some((_, s)) => s.len() + 1,
        _ => colno,
    }
}

