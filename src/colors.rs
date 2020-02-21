use blake2::Blake2b;
use hsl::HSL;
use rand_chacha::ChaChaRng;
use std::collections::BTreeMap;

fn rng_of_str(input: &str) -> ChaChaRng {
    use blake2::digest::{FixedOutput, Input};
    let hashed = <Blake2b as Default>::default()
        .chain(input.as_bytes())
        .chain(b"\0")
        .fixed_result();
    let condensed = <byteorder::LE as byteorder::ByteOrder>::read_u64(hashed.as_slice());
    <ChaChaRng as rand_chacha::rand_core::SeedableRng>::seed_from_u64(condensed)
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

fn color_and_opposite_by_direness(base: HSL) -> (HSL, HSL) {
    let opposite = base.rotate(180.0);
    if base.h <= 60.0 || base.h >= 240.0 {
        // base is in the 'dire' hue range
        (opposite, base)
    } else {
        (base, opposite)
    }
}

pub fn make_theme(input: &str) -> BTreeMap<&'static str, HSL> {
    use rand::distributions::{Distribution, Uniform};
    let mut rng = rng_of_str(input);
    let base = HSL {
        h: Uniform::new(0.0, 360.0).sample(&mut rng),
        s: Uniform::new(0.8, 1.0).sample(&mut rng),
        l: Uniform::new(0.5, 0.75).sample(&mut rng),
    };
    let cwd = base.rotate(120.0);
    let vcs = base.rotate(240.0);
    let (good_exit, bad_exit) = color_and_opposite_by_direness(cwd);
    let mut ret = <BTreeMap<_, _> as Default>::default();
    ret.insert("username", base);
    ret.insert("hostname", base.lighten(-0.2));
    ret.insert("prompt_char", base.rotate(-60.0));
    ret.insert("rprompt", base.rotate(-60.0).lighten(-0.2));
    ret.insert("cwd", cwd);
    ret.insert("good_exit", good_exit);
    ret.insert("bad_exit", bad_exit);
    ret.insert("vcs", vcs);
    ret
}
