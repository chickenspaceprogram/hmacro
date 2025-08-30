
struct FrontGrowingBuf {
    buf: Box<[u8]>,
    fst: usize,
}

fn alloc_boxed_slice<T: Clone>(nel: usize, default_val: &T) -> Box<[T]> {
    vec![default_val.clone(); nel].into_boxed_slice()
}

impl FrontGrowingBuf {
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
    pub fn peek_tok(&self) -> Option<&[u8]> {
        let buf = self.as_slice();
        if buf.len() == 0 {
            return None;
        }
        if buf[0] == '\\' {
        }
    }
}

enum Token<'a> {
    Macro(&'a [u8]),
    Text,
    LazyScope(&'a [u8]),
    GreedyScope(&'a [u8]),
    Error(usize),
}
