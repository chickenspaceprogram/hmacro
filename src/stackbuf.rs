
// this exists so i can replace it with something saner in future
// given the frequency of pushing to the front of this, O(n) is really bad!
//
// I originally made this a Vec<&str> (hence the name) but eventually realized
// that due to how I'd done things I needed it to maintain ownership.
//
// The design of this is awful but hey, it works
#[derive(Clone, Debug)]
pub struct StackBuf {
    buf: String
}

#[derive(Clone, Debug)]
pub enum Token {
    Scope(String),
    Macro(String),
    Text(String),
    EscChr(char),
    Error(&'static str),
}

fn is_important_char(ch: char) -> bool {
    ch == '\\' || ch == '{'
}

impl StackBuf {
    pub fn from_str(st: String) -> Self {
        StackBuf {
            buf: st
        }
    } 
    pub fn empty(&self) -> bool {
        return self.buf.len() == 0;
    }
    pub fn push(&mut self, st: &str) {
        self.buf.insert_str(0, st);
    }
    pub fn pop_tok(&mut self, try_for_scope: bool) -> Option<Token> {
        if self.buf.len() == 0 {
            return None;
        }
        let fstchr = self.buf.chars().next().unwrap(); // should never have empty strs
        match (fstchr, try_for_scope) {
            ('\\', _) => {
                eprintln!("macro");
                match self.buf[1..].chars().next() {
                    all@Some('\\' | '$' | '\n' | '{' | '}') => {
                        self.buf = self.buf.split_off(2);
                        return Some(Token::EscChr(all.unwrap()));
                    }
                    Some(chr) => {
                        if !(chr.is_ascii_alphabetic() || chr == '-' || chr == '_') {
                            return Some(Token::Error("Macro name must start with an alphabetic character, `-', or `_'"));
                        }
                        let mut tmp = self.buf.split_off(self.buf[1..].find(|c: char| !(c.is_ascii_alphanumeric() || c == '-' || c == '_')).unwrap() + 1);
                        std::mem::swap(&mut tmp, &mut self.buf);
                        return Some(Token::Macro(tmp));
                    },
                    None =>
                        return Some(Token::Error("Macro signifier `\\' found before end of file")),
                }
            },
            ('{', true) => {
                eprintln!("scope");
                let mut matcher = BracketMatchFinder::new();
                if let Some(res) = self.buf[1..].find(|ch| matcher.advance(ch)) {
                    let scp = self.buf[1..res + 1].to_string();
                    self.buf = self.buf.split_off(res + 2);
                    return Some(Token::Scope(scp));
                }
                else {
                    return Some(Token::Error("Mismatched brackets: Failed to find closing bracket"));
                }
            },
            ('{', _) => {
                eprintln!("esc-lbrack");
                // if we don't want to parse a scope, just auto-escape brackets
                self.buf = self.buf.split_off(1);
                return Some(Token::EscChr('{'));
            },
            _ => {
                eprintln!("nothin");
                if let Some(ind) = self.buf.find(|ch| is_important_char(ch)) {
                    let mut tmp = self.buf.split_off(ind);
                    std::mem::swap(&mut tmp, &mut self.buf);
                    return Some(Token::Text(tmp));
                }
                else {
                    let mut newbuf = String::new();
                    std::mem::swap(&mut self.buf, &mut newbuf);
                    return Some(Token::Text(newbuf));
                }
            },
        }
    }
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

