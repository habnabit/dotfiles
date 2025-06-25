use std::collections::BTreeMap;

use blake2::Blake2b512;
use hsl::HSL;
use rand_chacha::ChaChaRng;

fn rng_of_str(input: &str) -> ChaChaRng {
    use blake2::Digest;
    let hashed = Blake2b512::new()
        .chain_update(input.as_bytes())
        .chain_update(b"\0")
        .finalize();
    let mut condensed = [0u8; 8];
    condensed.copy_from_slice(&hashed.as_slice()[..8]);
    let condensed = u64::from_le_bytes(condensed);
    <ChaChaRng as rand_chacha::rand_core::SeedableRng>::seed_from_u64(condensed)
}

#[expect(dead_code)]
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
        HSL {
            h: (self.h + degrees) % 360.0,
            ..self
        }
    }

    fn saturate(self, amount: f64) -> HSL {
        HSL {
            s: clamp_0_1(self.s + amount),
            ..self
        }
    }

    fn lighten(self, amount: f64) -> HSL {
        HSL {
            l: clamp_0_1(self.l + amount),
            ..self
        }
    }
}

fn color_and_opposite_by_direness(base: HSL) -> (HSL, HSL) {
    let opposite = base.rotate(180.0);
    if base.h <= 80.0 || base.h >= 260.0 {
        // base is in the 'dire' hue range
        (opposite, base)
    } else {
        (base, opposite)
    }
}

pub fn make_theme(input: &str) -> BTreeMap<&'static str, HSL> {
    use rand::distributions::{Distribution, Uniform};
    let mut rng = rng_of_str(input);
    let base = loop {
        let ret = HSL {
            h: Uniform::new(0.0, 360.0).sample(&mut rng),
            s: Uniform::new(0.8, 1.0).sample(&mut rng),
            l: Uniform::new(0.5, 0.75).sample(&mut rng),
        };
        if ret.l < 0.55 {
            continue;
        }
        break ret;
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
