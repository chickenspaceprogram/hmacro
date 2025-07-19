#[derive(Clone, Debug)]
pub struct StackBuf<'a> {
    buf: Vec<&'a str>
}

#[derive(Clone, Debug)]
pub enum Token<'a> {
    Scope(StackBuf<'a>),
    Macro(String),
    Text(StackBuf<'a>),
    EscChr(char),
    Error(&'static str),
}

fn is_important_char(ch: char) -> bool {
    ch == '\\' || ch == '{'
}

// Effectively a string that has O(1) push_front
impl<'a> StackBuf<'a> {
    pub fn new() -> Self {
        StackBuf {
            buf: Vec::new(),
        }
    }
    pub fn from_vec(vec: Vec<&'a str>) -> Option<Self> {
        for elem in &vec {
            if elem.len() == 0 {
                return None;
            }
        }
        Some(StackBuf {
            buf: vec
        })
    } 
    pub fn from_vec_unchecked(vec: Vec<&'a str>) -> Self {
        StackBuf {
            buf: vec
        }
    }
    pub fn len(&self) -> usize {
        let mut total: usize = 0;
        for elem in self.buf.as_slice() {
            total += elem.len();
        }
        return total;
    }
    pub fn empty(&self) -> bool {
        if self.buf.len() > 0 {
            return false;
        }
        return true;
    }
    pub fn count_newlines(&self) -> usize {
        let mut total: usize = 0;
        for elem in self.buf.as_slice() {
            total += elem.chars().filter(|c| *c == '\n').count();
        }
        return total;
    }
    pub fn count_chrs_after_newline(&self) -> usize {
        let mut total: usize = 0;
        for elem in self.buf.as_slice() {
            if let Some(num) = elem.rfind('\n') {
                total += elem.len() - num;
                break;
            }
            total += elem.len();
        }
        return total;
    }
    pub fn push(&mut self, st: &'a str) {
        self.buf.push(st);
    }
    pub fn inc_loc(&self, loc: &mut crate::parser::Location) {
        let nls = self.count_newlines();
        loc.row += nls;
        if nls == 0 {
            loc.col += self.len();
        }
        else {
            loc.col = 1 + self.count_chrs_after_newline();
        }
    }
    pub fn append_to_str(&self, outbuf: &mut String) {
        let mut index = self.buf.len();
        while index > 0 {
            index -= 1;
            outbuf.push_str(self.buf[index]);
        }
    }
    pub fn pop_tok(&mut self, try_for_scope: bool) -> Option<Token<'a>> {
        let last = self.buf.last_mut()?;
        let fstchr = last.chars().next().unwrap(); // should never have empty strs
        match (fstchr, try_for_scope) {
            ('\\', _) => {
                *last = &last[1..];
                match last.chars().next() {
                    all@Some('\\' | '$' | '\n' | '{' | '}') => {
                        *last = &last[1..];
                        return Some(Token::EscChr(all.unwrap()));
                    }
                    Some(chr) => {
                        if !(chr.is_ascii_alphabetic() || chr == '-' || chr == '_') {
                            return Some(Token::Error("Macro name must start with an alphabetic character, `-', or `_'"));
                        }
                        return Some(Token::Macro(self.pop_macro_name()));
                    },
                    None =>
                        return Some(Token::Error("Macro signifier `\\' found before end of file")),
                }
            },
            ('{', true) => {
                *last = &last[1..];
                let mut matcher = BracketMatchFinder::new();
                if let Some(res) = self.pop_at_pattern(|ch| matcher.advance(ch)) {
                    let fin = self.buf.last_mut().unwrap();
                    *fin = &fin[1..]; // trim bracket off
                    return Some(Token::Scope(res));
                }
                else {
                    return Some(Token::Error("Mismatched brackets: Failed to find closing bracket"));
                }
            },
            ('{', _) => {
                // if we don't want to parse a scope, just auto-escape brackets
                *last = &last[1..];
                return Some(Token::EscChr('{'));
            },
            _ => {
                if let Some(sbuf) = self.pop_at_pattern(|ch| is_important_char(ch)) {
                    return Some(Token::Text(sbuf));
                }
                else {
                    let mut newbuf = StackBuf::new();
                    std::mem::swap(self, &mut newbuf);
                    return Some(Token::Text(newbuf));
                }
            },
        }
    }
    // Scans the StackBuf for a match to the pattern. If one gets found, splits
    // all characters before it that don't match the pattern off the front of
    // the StackBuf and returns a new StackBuf containing those chars.
    //
    // If no match is found, returns None.
    pub fn pop_at_pattern<F: FnMut(char) -> bool>(&mut self, mut pat: F) -> Option<StackBuf<'a>> {
        let mut index = self.buf.len();
        while index > 0 {
            index -= 1;
            if let Some(i) = self.buf[index].find(&mut pat) {
                if i == 0 {
                    return Some(StackBuf::from_vec_unchecked(self.buf.split_off(index)));
                }
                let mut out = StackBuf::new();
                out.buf.push(&self.buf[index][..i]);
                self.buf[index] = &self.buf[index][i..];
                push_split_off(&mut self.buf, &mut out.buf, index + 1);
                return Some(out);
            }
            
        }
        return None;
    }
    fn pop_macro_name(&mut self) -> String {
        let mut index = self.buf.len();
        let mut outbuf: String = '\\'.to_string();
        while index > 0 {
            index -= 1;
            if let Some(i) = self.buf[index].find(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '-' || ch == '_')) {
                outbuf += &self.buf[index][..i];
                let slice = &self.buf[index][i..];
                self.buf[index] = slice;
                break;
            }
            outbuf += self.buf.pop().unwrap();
        }
        return outbuf;
    }
}

fn push_split_off<T: Clone>(v1: &mut Vec<T>, v2: &mut Vec<T>, index: usize) {
    let mut i = index;
    while i < v1.len() {
        v2.push(v1[i].clone());
        i += 1;
    }
    v1.truncate(index - 1);
}

#[derive(Clone, Copy, Debug)]
struct BracketMatchFinder {
    nbrack: usize,
    next_escaped: bool,
}

impl BracketMatchFinder {
    fn new() -> Self {
        BracketMatchFinder {
            nbrack: 1,
            next_escaped: false,
        }
    }
    fn advance(&mut self, ch: char) -> bool {
        if self.next_escaped {
            self.next_escaped = false;
            return false;
        }
        match ch {
            '{' => self.nbrack += 1,
            '}' => {
                self.nbrack -= 1;
                if self.nbrack == 0 {
                    return true;
                }
            },
            _ => (),
        }
        return false;
    }
}

