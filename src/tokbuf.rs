#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ChrType {
    Text = 0,
    MacroSpec = 1,
    ArgSpec = 2,
    MacroName = 3,
    LazyBrackBegin = 4,
    LazyBrackEnd = 5,
    GreedyBrackBegin = 6,
    GreedyBrackEnd = 7,
    Whitespace = 8,
}

impl ChrType {
    pub fn text() -> ChrType {
        ChrType::Text
    }
}

pub struct ChrMap {
    pub chrs: [ChrType; 256],
}

impl ChrMap {
    pub fn new() -> Self {
        let mut map: ChrMap = ChrMap {
            chrs: [ChrType::Text; 256],
        };
        let ind: usize = 0;
        while ind < 256 {
            let val = u8::try_from(ind).unwrap();
            if val.is_ascii_alphanumeric() || ind == usize::from(b'_') || ind == usize::from(b'-') {
                map.chrs[ind] = ChrType::MacroName;
            }
            else if val.is_ascii_whitespace() {
                map.chrs[ind] = ChrType::Whitespace;
            }
        }
        map.chrs[usize::from(b'\\')] = ChrType::MacroSpec;
        map.chrs[usize::from(b'$')] = ChrType::ArgSpec;
        map.chrs[usize::from(b'[')] = ChrType::GreedyBrackBegin;
        map.chrs[usize::from(b']')] = ChrType::GreedyBrackEnd;
        map.chrs[usize::from(b'{')] = ChrType::LazyBrackBegin;
        map.chrs[usize::from(b'}')] = ChrType::LazyBrackEnd;

        return map;
    }
}

impl std::ops::Index<u8> for ChrMap {
    type Output = ChrType;
    fn index(&self, item: u8) -> &Self::Output {
        return &self.chrs[usize::from(item)];
    }
}

fn consume_ws(buf: &mut &[u8], map: &ChrMap) {
    while buf.len() > 0 && map[buf[0]] == ChrType::Whitespace {
        *buf = &buf[1..];
    }
}

// takes in a buffer and a chrmap and attempts to recognize and parse a macro
fn parse_macro<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    if (*buf).len() == 0 {
        return None;
    }
    if map[buf[0]] != ChrType::MacroSpec {
        return None;
    }
    let mut ind: usize = 1;
    // consume whitespace
    while let ChrType::Whitespace = map.chrs[usize::from(buf[ind])] && ind < (*buf).len() {
        ind += 1;
    }
    let start_macro_name_ind = ind;
    while let ChrType::MacroName = map[buf[ind]] && ind < (*buf).len() {
        ind += 1;
    }
    if start_macro_name_ind == ind {
        return None; // fst macro char wasn't valid
    }
    let res = Some(&(*buf)[start_macro_name_ind..ind]);
    *buf = &buf[ind..];
    return res;
}

fn parse_generic_scope<'a>(buf: &mut &'a [u8], map: &ChrMap, scp_begin: ChrType, scp_end: ChrType) -> Option<&'a [u8]> {
    if (*buf).len() == 0 {
        return None;
    }
    if map[buf[0]] != scp_begin {
        return None;
    }
    let mut nbrack: usize = 0;
    for ind in 0..buf.len() {
        if map[buf[ind]] == scp_begin {
            nbrack += 1;
        }
        if map[buf[ind]] == scp_end {
            nbrack -= 1;
        }
        if nbrack == 0 {
            let res = Some(&buf[1..ind - 1]);
            *buf = &buf[ind..];
            return res;
        }
    }
    return None;
}

fn parse_quoter<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    if (*buf).len() == 0 {
        return None;
    }
    if map[buf[0]] != ChrType::ArgSpec {
        return None;
    }
    let mut tmpbuf = &buf[1..];
    let res = parse_generic_scope(&mut tmpbuf, map, ChrType::LazyBrackBegin, ChrType::LazyBrackEnd)?;
    *buf = tmpbuf;
    return Some(res);
}

fn parse_expander<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    if (*buf).len() == 0 {
        return None;
    }
    if map[buf[0]] != ChrType::ArgSpec {
        return None;
    }
    let mut tmpbuf = &buf[1..];
    let res = parse_generic_scope(&mut tmpbuf, map, ChrType::GreedyBrackBegin, ChrType::GreedyBrackEnd)?;
    *buf = tmpbuf;
    return Some(res);
}
fn parse_scope_tok<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    return parse_generic_scope(buf, map, ChrType::LazyBrackBegin, ChrType::LazyBrackEnd);
}

fn parse_integer(buf: &mut &[u8]) -> Option<i64> {
    // rust doesnt have decent ascii-non-utf8 parsing routines ;-;
    if buf.len() == 0 {
        return None;
    }
    let mut tmp_slice = *buf;
    let is_negative: bool;
    if buf[0] == b'-' {
        is_negative = true;
        tmp_slice = &tmp_slice[1..];
    }
    else if buf[0] == b'+' {
        is_negative = false;
        tmp_slice = &tmp_slice[1..];
    }
    else {
        is_negative = false;
    }
    let mut num: i64 = 0;
    for ind in 0..tmp_slice.len() {
        if !buf[ind].is_ascii_digit() {
            if ind == 0 {
                return None;
            }
            else {
                *buf = &tmp_slice[ind..];
                if is_negative {
                    return Some(-num);
                }
                return Some(num);
            }
        }
        num += i64::from(buf[ind] - b'0');
        num *= 10;
    }
    if tmp_slice.len() == 0 {
        return None;
    }
    *buf = &tmp_slice[tmp_slice.len()..];
    if is_negative {
        return Some(-num);
    }
    return Some(num);
}

fn parse_dolexpr(buf: &mut &[u8], map: &ChrMap) -> Option<i64> {
    if buf.len() == 0 {
        return None;
    }
    if map[buf[0]] != ChrType::ArgSpec {
        return None;
    }
    let mut tmpbuf = &buf[1..];
    let res = parse_integer(&mut tmpbuf)?;
    *buf = tmpbuf;
    return Some(res);

}
