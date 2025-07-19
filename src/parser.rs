use crate::stackbuf::{Token, StackBuf};
use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

#[derive(Clone, Debug)]
pub struct Error {
    pub msg: String,
    pub loc: Location,
}



#[derive(Clone, Debug)]
enum MacroDefElem {
    Txt(String),
    ArgNo(usize),
}

#[derive(Clone, Debug)]
enum MacroArg {
    Macro(String),
    Scope(String),
}


#[derive(Clone, Debug)]
struct Macro {
    name: String,
    args: Vec<MacroArg>
}

impl Macro {
    fn new() -> Self {
        Macro {
            name: String::new(),
            args: Vec::new(),
        }
    }
}

struct FileIncluder {
}

const IMPORTANT_CHARS: &'static str = "\\$";

type MacroMap = HashMap<String, Vec<MacroDefElem>>;

type InbuiltMacro = fn (&Macro, &mut MacroMap, &mut FileIncluder) -> Result<String, Vec<String>>;

fn is_macro_name(txt: &str) -> bool {
    if txt.len() == 0 {
        return false;
    }
    let fst = txt.chars().next().unwrap();
    if !(fst.is_ascii_alphabetic() || fst == '-' || fst == '_') {
        return false;
    }
    if let None = txt.find(|ch: char| !(ch.is_ascii_alphanumeric() || ch == '-' || ch == '_')) {
        return true; // found no bad chars
    }
    return false;
}

fn parse_def_arg(mut txt: &str) -> Result<Vec<MacroDefElem>, Vec<String>> {
    let mut outbuf = Vec::new();
    let mut errvec = Vec::new();
    while txt.len() > 0 {
        let chr = txt.chars().next().unwrap();
        match (chr, txt.chars().next()) {
            ('$', _) => {
                txt = &txt[1..];
                if let Some(end) = txt.find(|ch: char| !ch.is_numeric()) {
                    if end == 0 {
                        errvec.push("Macro arg-specifier `$' must be followed by numeric characters".to_string());
                    }
                    else {
                        outbuf.push(MacroDefElem::ArgNo(txt[..end].parse().unwrap()));
                    }
                }
            }
            ('\\', Some('$')) => {
                txt = &txt[2..];
                outbuf.push(MacroDefElem::Txt("\\$".to_string()));
            },
            _ => {
                if let Some(ind) = txt.find(IMPORTANT_CHARS) {
                    outbuf.push(MacroDefElem::Txt(txt[..ind].to_string()));
                    txt = &txt[ind..];
                }
                else {
                    outbuf.push(MacroDefElem::Txt(txt.to_string()));
                    txt = &txt[txt.len()..];
                }
            },
        }
    }
    if errvec.len() == 0 {
        return Ok(outbuf);
    }
    else {
        return Err(errvec);
    }
}


fn expand_def(mac: &Macro, map: &mut MacroMap, _: &mut FileIncluder) -> Result<String, Vec<String>> {
    let macname = match &mac.args[0] {
        MacroArg::Macro(nm) => nm.to_string(),
        MacroArg::Scope(scp) => "\\".to_string() + scp,
    };
    if !is_macro_name(&macname[1..]) {
        let mut evec = Vec::new();
        evec.push("Attempted to define macro `".to_string() + macname.as_str() + "\', which is an invalid macro name");
        return Err(evec);
    }
    let exp = match &mac.args[1] {
        MacroArg::Macro(nm) => parse_def_arg(nm)?,
        MacroArg::Scope(scp) => parse_def_arg(scp)?,
    };
    map.insert(macname, exp);
    return Ok(String::new());
}

static INBUILT_MACRO_MAP: LazyLock<HashMap<&'static str, (usize, InbuiltMacro)>> = LazyLock::new(|| {
    let mut map: HashMap<&'static str, (usize, InbuiltMacro)> = HashMap::new();
    map.insert("\\def", (2, expand_def));
    return map;
});

fn expand_macro(mac: &Macro, macro_map: &mut MacroMap, file_inc: &mut FileIncluder) -> Result<String, Vec<String>> {
    if let Some((min_args, func)) = INBUILT_MACRO_MAP.get(mac.name.as_str()) {
        if mac.args.len() < *min_args {
            let mut evec = Vec::new();
            evec.push("Inbuilt macro `".to_string() + mac.name.as_str() + "\' requires at least " + min_args.to_string().as_str() + " arguments, and " + mac.args.len().to_string().as_str() + " were provided");
            return Err(evec);
        }
        return func(mac, macro_map, file_inc);
    }
    if let Some(def_vec) = macro_map.get(mac.name.as_str()) {
        let mut outbuf = String::new();
        for elem in def_vec {
            match elem {
                MacroDefElem::Txt(txt) => outbuf += txt,
                MacroDefElem::ArgNo(no) => {
                    if mac.args.len() < *no {
                        let mut evec = Vec::new();
                        evec.push("Macro `".to_string() + mac.name.as_str() + "\' was not provided enough arguments");
                        return Err(evec);
                    }
                    match &mac.args[no - 1] {
                        MacroArg::Macro(name) => outbuf += name.as_str(),
                        MacroArg::Scope(scp) => outbuf += scp.as_str(),
                    }
                },
            }
        }
        return Ok(outbuf);
    }
    let mut evec = Vec::new();
    evec.push("Macro `".to_string() + mac.name.as_str() + "\' was not defined and is not an inbuilt macro");
    return Err(evec);
}

// escape chars will still be escaped after this!
fn parse_internal(
    buf: &mut StackBuf,
    loc: &mut Location,
    macros: &mut HashMap<String, Vec<MacroDefElem>>
) -> Result<String, Vec<Error>> {
    let mut current_macro = Macro::new();
    let mut out = String::new();
    let mut in_macro = false;
    let mut expand_macro = false;

    while let Some(tok) = buf.pop_tok(!in_macro) {
        let old_loc = *loc;
        match tok {
            Token::Scope(scp) => {
                scp.inc_loc(loc);
                assert!(in_macro, "Internal parser error: can only parse scopes after macros!");
                let mut st = String::new();
                scp.append_to_str(&mut st);
                current_macro.args.push(MacroArg::Scope(st));
            },
            Token::Macro(name) => {
                loc.col += name.len();
                if in_macro {
                    if name == "\\begin" {
                        // greedily expand until \end
                        let res = parse_internal(buf, loc, macros)?;
                        if buf.empty() {
                            let mut evec = Vec::new();
                            evec.push(Error {
                                msg: "Found \\begin, expected matching \\end".to_string(),
                                loc: *loc,
                            });
                            return Err(evec);
                        }
                        current_macro.args.push(MacroArg::Scope(res));
                    }
                    else {
                        current_macro.args.push(MacroArg::Macro(name));
                    }
                }
                else {
                    if name == "\\end" {
                        return Ok(out);
                    }
                    in_macro = true;
                    current_macro = Macro::new();
                    current_macro.name = name;
                }
            },
            Token::Text(txtbuf) => {
                if in_macro {
                    in_macro = false; // macro has ended
                    expand_macro = true; // need to expand it
                }
                txtbuf.inc_loc(loc);
                txtbuf.append_to_str(&mut out);
            },
            Token::EscChr(ch) => {
                if in_macro {
                    in_macro = false; // macro has ended
                    expand_macro = true; // need to expand it
                }
                if ch == '\n' {
                    loc.row += 1;
                    loc.col = 1;
                }
                else {
                    loc.col += 2;
                    out.push('\\');
                    out.push(ch);
                }
            },
            Token::Error(e) => {
                let mut evec = Vec::new();
                evec.push(Error {msg: e.to_string(), loc: *loc});
                return Err(evec);
            },
        }
        if !expand_macro {
            continue;
        }
        expand_macro = false;
        match crate::parser::expand_macro(&current_macro, macros, &mut FileIncluder {}) {
            Ok(txt) => buf.push(txt.as_str()),
            Err(es) => {
                let mut outvec = Vec::new();
                outvec.reserve(es.len());
                for err in es {
                    outvec.push(Error {msg: err, loc: old_loc});
                }
                return Err(outvec);
            },
        }
    }
    todo!();
}

