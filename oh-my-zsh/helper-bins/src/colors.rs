use ansi_term;
use byteorder;
use hsl::HSL;
use rand;
use regex;
use sha1::Sha1;
use std::collections::BTreeMap;

lazy_static! {
    static ref COLORHASH_PATTERN: regex::Regex = regex::Regex::new(r"(?ix)
        \{(?P<to_hash>.*?)\}
    ").unwrap();
}

fn pick_color(choices: &[u8], allow_all: bool) -> u8 {
    for &c in choices {
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

pub fn colorhash(input: &[u8], allow_all: bool) -> String {
    let mut h = Sha1::new();
    h.update(input);
    h.update(&[b'\n']);
    format!("{:03}", pick_color(&h.digest().bytes(), allow_all))
 }

pub fn parse_and_colorhash(input: &str, allow_all: bool) -> String {
    COLORHASH_PATTERN.replace_all(input, |capture: &regex::Captures| {
        let input_str = match capture.name("to_hash") {
            Some(m) => m.as_str(),
            None => return "".into(),
        };
        let color = colorhash(input_str.as_bytes(), allow_all);
        format!("%{{$FG[${}]%}}{}%{{$reset_color%}}", color, input_str)
    }).into()
}

fn rng_of_str(input: &str) -> rand::ChaChaRng {
    let mut h = Sha1::new();
    h.update(input.as_bytes());
    h.update(b"\0");
    let hashed_u8 = h.digest().bytes();
    let mut hashed_u32 = [0u32; 5];
    <byteorder::LE as byteorder::ByteOrder>::read_u32_into(&hashed_u8, &mut hashed_u32);
    <rand::ChaChaRng as rand::SeedableRng<_>>::from_seed(&hashed_u32)
}

trait HslExt {
    fn rotate(self, degrees: f64) -> Self;
    fn saturate(self, amount: f64) -> Self;
    fn lighten(self, amount: f64) -> Self;
}

fn clamp_0_1(mut f: f64) -> f64 {
    if f > 1.0 {
        f = 1.0;
    } else if f < 0.0 {
        f = 0.0;
    }
    f
}

impl HslExt for HSL {
    fn rotate(self, degrees: f64) -> HSL {
        HSL { h: (self.h + degrees) % 360.0, ..self }
    }

    fn saturate(self, amount: f64) -> HSL {
        HSL { s: clamp_0_1(self.s + amount), ..self }
    }

    fn lighten(self, amount: f64) -> HSL {
        HSL { l: clamp_0_1(self.l + amount), ..self }
    }
}

fn color_of_hsl(color: HSL) -> ansi_term::Color {
    let (r, g, b) = color.to_rgb();
    ansi_term::Color::RGB(r, g, b)
}

fn color_and_opposite_by_direness(base: HSL) -> (HSL, HSL) {
    let opposite = base.rotate(180.0);
    if base.h <= 60.0 || base.h >= 240.0 {
        // base is in the 'dire' hue range
        (opposite, base)
    } else {
        (base, opposite)
    }
}

pub fn make_theme(input: &str) -> BTreeMap<&'static str, String> {
    use rand::distributions::{Range, IndependentSample};
    let mut rng = rng_of_str(input);
    let base = HSL {
        h: Range::new(0.0, 360.0).ind_sample(&mut rng),
        s: Range::new(0.8, 1.0).ind_sample(&mut rng),
        l: Range::new(0.5, 0.75).ind_sample(&mut rng),
    };
    let cwd = base.rotate(120.0);
    let vcs = base.rotate(240.0);
    let (good_exit, bad_exit) = color_and_opposite_by_direness(cwd);
    let mut ret = <BTreeMap<_, _> as Default>::default();
    {
        let mut add = |name, hsl| {
            let color = color_of_hsl(hsl).normal().prefix();
            ret.insert(name, format!("{}", color));
        };
        add("username", base);
        add("hostname", base.lighten(-0.2));
        add("prompt_char", base.rotate(-60.0));
        add("rprompt", base.rotate(-60.0).lighten(-0.2));
        add("cwd", cwd);
        add("good_exit", good_exit);
        add("bad_exit", bad_exit);
        add("vcs", vcs);
    }
    ret
}
