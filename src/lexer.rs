#[derive(Debug, Clone)]
pub enum TokenVal<'a> {
    Text(&'a str),
    Macro(&'a str),
    Arg(usize),
    LBrack,
    RBrack,
    EscChr(u8),
    Error(&'static str),
}

#[derive(Debug, Copy, Clone)] // justifyable, this is small
pub struct Location {
    row: usize,
    col: usize,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub tok: TokenVal<'a>,
    pub loc: Location,
}

const IMPORTANT_CHARS: [char; 4] = ['\\', '{', '}', '$'];

fn count_newlines(txt: &str) -> usize {
    txt.chars().filter(|ch| *ch == '\n').count()
}

pub fn pop_tok<'a>(txt: &mut &'a str, loc: &mut Location) -> Option<Token<'a>> {
    if txt.len() == 0 {
        return None;
    }
    if let Some(len) = txt.find(IMPORTANT_CHARS) {
        if len != 0 {
            let result = Token{tok: TokenVal::Text(&txt[..len]), loc: *loc};
            *txt = &txt[len..];
            let nls = count_newlines(&txt[..len]);
            loc.row += nls;
            if nls == 0 {
                loc.col += len;
            }
            else {
                loc.col = len - txt[..len].rfind('\n').unwrap()
            }
            return Some(result);
        }
        if txt.as_bytes()[0] == b'{' {
            *txt = &txt[1..];
            let res = Token {tok: TokenVal::LBrack, loc: *loc};
            loc.col += 1;
            return Some(res);
        }
        if txt.as_bytes()[0] == b'}' {
            *txt = &txt[1..];
            let res = Token {tok: TokenVal::RBrack, loc: *loc};
            loc.col += 1;
            return Some(res);
        }
        if txt.len() == 1 {
            let tokstr;
            if txt.as_bytes()[1] == b'$' {
                tokstr = "Argument specifier `$' found before end of file";
            }
            else {
                tokstr = "Macro specifier `\\' found before end of file";
            }
            let tok = Token {tok: TokenVal::Error(tokstr), loc: *loc};
            loc.col += 1;
            *txt = &txt[1..];
            return Some(tok);
        }
        if txt.as_bytes()[1] == b'$' {
            if let Some(res) = txt[1..].find(|c: char| !c.is_ascii_digit()) {
                let retval = Token {tok: TokenVal::Arg(txt[1..res + 1].parse().unwrap()), loc: *loc};
                loc.col += res + 1;
                *txt = &txt[res + 1..];
                return Some(retval);
            }
            let tmp = Token {tok: TokenVal::Arg(txt[1..].parse().unwrap()), loc: *loc};
            loc.col += txt.len() - 1;
            *txt = &txt[txt.len()..];
            return Some(tmp);
        }
        if txt.as_bytes()[2] == b'\\' ||
            txt.as_bytes()[2] == b'\n' ||
            txt.as_bytes()[2] == b'$' ||
            txt.as_bytes()[2] == b'{' ||
            txt.as_bytes()[2] == b'}' {
            let retval = Token {tok: TokenVal::EscChr(txt.as_bytes()[2]), loc: *loc};
            loc.col += 2;
            *txt = &txt[2..];
            return Some(retval);
        }
        // know first char is '\\'
        if let Some(res) = txt[1..].find(|c: char| !(c.is_ascii_alphanumeric() || c == '-' || c == '_')) {
            let retval = Token {tok: TokenVal::Macro(&txt[1..res + 1]), loc: *loc};
            loc.col += res + 1;
            *txt = &txt[res + 1..];
            return Some(retval);
        }
        else {
            let retval = Token {tok: TokenVal::Macro(&txt[1..]), loc: *loc};
            loc.col += txt.len() + 1;
            *txt = &txt[txt.len() + 1..];
            return Some(retval);
        }
    }
    else {
        let result = Token {tok: TokenVal::Text(txt), loc: *loc};
        loc.col += txt.len();
        *txt = &txt[txt.len()..]; // empty slice, next call will fail
        return Some(result);
    }
}

