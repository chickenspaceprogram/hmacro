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

// takes in a buffer and a chrmap and attempts to recognize and parse a macro
fn parse_macro<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    if (*buf).len() == 0 {
        return None;
    }
    if map.chrs[usize::from((*buf)[0])] != ChrType::MacroSpec {
        return None;
    }
    let mut ind: usize = 1;
    // consume whitespace
    while let ChrType::Whitespace = map.chrs[usize::from(buf[ind])] && ind < (*buf).len() {
        ind += 1;
    }
    let start_macro_name_ind = ind;
    while let ChrType::MacroName = map.chrs[usize::from(buf[ind])] && ind < (*buf).len() {
        ind += 1;
    }
    if start_macro_name_ind == ind {
        return None; // fst macro char wasn't valid
    }
    return Some(&(*buf)[start_macro_name_ind..ind]);
}

fn parse_lazyscope<'a>(buf: &mut &'a [u8], map: &ChrMap) -> Option<&'a [u8]> {
    todo!()
}
