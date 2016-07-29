use sha1::Sha1;

use super::errors::PromptResult as Result;

fn pick_color(choices: Vec<u8>, allow_all: bool) -> u8 {
    for c in choices {
        if allow_all || match c {
            22 ... 51 => true,
            58 ... 230 => true,
            _ => false,
        } {
            return c;
        }
    }
    248
}

pub fn colorhash(input: &[u8], allow_all: bool) -> Result<String> {
    let mut h = Sha1::new();
    h.update(input);
    h.update(&[b'\n']);
    Ok(format!("{:03}", pick_color(h.digest(), allow_all)))
 }
