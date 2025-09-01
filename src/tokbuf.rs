use std::sync::LazyLock;
use std::path::PathBuf;
use regex::bytes;
pub struct FrontGrowingBuf {
    buf: Box<[u8]>,
    fst: usize,
}

pub struct LocationInfo {
    file_path: PathBuf,
    row: usize,
    col: usize,
}

struct LocationTag {
    loc: LocationInfo,
    // number of bytes that must be popped to get to `loc`
    ignored_bytes: usize,
}


pub struct TokBuf {
    buf: FrontGrowingBuf,
    fname_stack: Vec<(LocationInfo, usize)>,
}

fn alloc_boxed_slice<T: Copy>(nel: usize, default_val: &T) -> Box<[T]> {
    vec![*default_val; nel].into_boxed_slice()
}

static macro_regex: LazyLock<bytes::Regex> = LazyLock::new(||
    bytes::Regex::new(r"\[[:space:]]*([a-zA-Z_][[:word:]]*)").unwrap()
);

static esc_ws_regex: LazyLock<bytes::Regex> = LazyLock::new(||
    bytes::Regex::new("\\\n[[:space:]]*").unwrap()
);

const esc_chrs: [u8; 6] = [b'\\', b'$', b'{', b'}', b'[', b']'];

impl<'a> FrontGrowingBuf {
    pub fn new() -> Self {
        FrontGrowingBuf {
            buf: Box::new([]),
            fst: 0,
        }
    }
    pub fn push_front(&mut self, txt: &[u8]) {
        if txt.len() > self.fst {
            self.reserve(self.fst + txt.len());
        }
        let new_fst = self.fst - txt.len();
        self.buf[new_fst..self.fst].copy_from_slice(txt);
        self.fst = new_fst;
    }
    pub fn reserve(&mut self, sz: usize) {
        let mut tmp: Box<[u8]> = alloc_boxed_slice(sz.next_power_of_two(), &u8::from(0));
        let nels: usize = self.buf.len() - self.fst;
        let fst_el = tmp.len() - nels;
        tmp[fst_el..].copy_from_slice(&self.buf[self.fst..]);
        self.fst = fst_el;
        std::mem::swap(&mut self.buf, &mut tmp);
    }
    pub fn pop_front(&mut self, amt: usize) {
        self.fst += amt;
        if self.fst > self.buf.len() {
            self.fst = self.buf.len();
        }
    }
    pub fn len(&self) -> usize {
        self.buf.len() - self.fst
    }
    pub fn as_slice(&self) -> &[u8] {
        &self.buf[self.fst..]
    }
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.buf[self.fst..]
    }
}

impl<'a> TokBuf {
}

pub fn peek_tok<'a>(buf: &'a [u8], want_scope: bool) -> Option<(Token<'a>, usize)> {
    if buf.len() == 0 {
        return None;
    }
    if let Some(m) = esc_ws_regex.find(buf) {
        return Some((Token::EscWs, m.len()));
    }
    if buf.len() >= 2 && buf[0] == b'\\' && esc_chrs.contains(&buf[1]) {
        return Some((Token::EscChr(buf[1]), 2));
    }
    if let Some(caps) = macro_regex.captures(buf) {
        let res = caps.get(1).unwrap();
        return Some((Token::Macro(&buf[res.start()..res.end()]), caps.get(0).unwrap().len()));
    }
    if want_scope {
        if buf[0] == b'[' {
            return Some((Token::BeginGreedyScope, 1));
        }
        if buf[0] == b']' {
            return Some((Token::EndGreedyScope, 1));
        }
        if buf[0] == b'{' {
            if let Some(res) = match_brack(buf) {
                return Some((Token::LazyScope(&buf[1..res - 1]), res));
            }
            return Some((Token::Error("Unmatched bracket".to_string()), 0));
        }
    }
    if let Some(esc) = memchr::memchr(b'\\', buf) {
        return Some((Token::Text(&buf[..esc]), esc));
    }
    else {
        return Some((Token::Text(buf), buf.len()));
    }
}


fn match_brack(slice: &[u8]) -> Option<usize> {
    assert!(slice.len() > 0 && slice[0] == b'{', "Bad slice passed to match_brack()");
    let mut brack_count = 0;
    for brack in memchr::memchr2_iter(b'{', b'}', slice) {
        match slice[brack] {
            b'{' => brack_count += 1,
            b'}' => {
                brack_count -= 1;
                if brack_count == 0 {
                    return Some(brack + 1);
                }
            },
            _ => panic!("memchr caused an error"),
        }
    }
    return None;
}

pub enum Token<'a> {
    Macro(&'a [u8]),
    Text(&'a [u8]),
    BeginGreedyScope,
    EndGreedyScope,
    LazyScope(&'a [u8]),
    EscChr(u8),
    EscWs,
    Error(String),
}
