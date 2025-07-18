use std::core::Index;

#[derive(Clone, Debug)]
pub struct StackBuf<'a> {
    buf: Vec<&'a str>,
}

impl <'a>StackBuf<'a> {
    pub fn new() -> Self {
        StackBuf {buf: Vec::new()}
    }
    pub fn push(txt: &'a str) {
        buf.push(txt);
    }
}

impl Index for StackBuf {
    fn index(&self, mut index: Idx) -> char {
        for elem in buf {
            if index < elem.len() {
                return 
            }
            else {
                index -= elem.len();
            }
        }
    }
}

