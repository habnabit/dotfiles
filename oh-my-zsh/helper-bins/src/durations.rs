use std::{fmt, time};

fn float_precision(v: f64, sig_figs: usize) -> usize {
    let v = v.abs();
    if v == 0f64 {
        return 0;
    }
    let prec = (v.log10().floor() - (sig_figs as f64)) as isize + 1;
    if prec >= 0 {
        0
    } else {
        -prec as usize
    }
}

pub struct SigFigFloat(pub f64);

impl fmt::Display for SigFigFloat {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(prec) = f.precision() {
            write!(f, "{:.*}", float_precision(self.0, prec), self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

pub struct PrettyDuration(pub time::Duration);

const TIME_UNITS: [(&'static str, u64); 3] = [("d", 60 * 60 * 24), ("h", 60 * 60), ("m", 60)];

impl fmt::Display for PrettyDuration {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut written_first = false;
        let mut secs = self.0.as_secs();
        for &(ref unit, unit_secs) in &TIME_UNITS {
            let unit_count = secs / unit_secs;
            if unit_count == 0 {
                continue;
            }
            secs %= unit_secs;
            try!(write!(
                f,
                "{}{}{}",
                {
                    if written_first {
                        " "
                    } else {
                        ""
                    }
                },
                unit_count,
                unit
            ));
            written_first = true;
        }
        if written_first {
            if secs > 0 {
                try!(write!(f, " {}s", secs));
            }
        } else {
            let fsecs = secs as f64 + (self.0.subsec_nanos() as f64 / 1_000_000_000.);
            let prec = f.precision().unwrap_or(2);
            try!(write!(f, "{:.*}s", prec, SigFigFloat(fsecs)));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::{float_precision, PrettyDuration};

    parametrize_test! {test_float_precision_formatting, [
        (v: f64, s: usize, r: &'static str),
        (100.,      2, "100"),
        ( 10.,      2, "10"),
        (  1.,      2, "1.0"),
        (  0.,      2, "0"),
        (  0.1,     2, "0.10"),
        (123.456,   2, "123"),
        ( 23.456,   2, "23"),
        (  3.456,   2, "3.5"),
        (  0.456,   2, "0.46"),
        (  0.056,   2, "0.056"),
        (  0.00078, 2, "0.00078"),

        (100.,      3, "100"),
        ( 10.,      3, "10.0"),
        (  1.,      3, "1.00"),
        (  0.,      3, "0"),
        (  0.1,     3, "0.100"),
        (123.456,   3, "123"),
        ( 23.456,   3, "23.5"),
        (  3.456,   3, "3.46"),
        (  0.456,   3, "0.456"),
        (  0.056,   3, "0.0560"),
        (  0.00078, 3, "0.000780"),
    ], {
        assert_eq!(format!("{:.*}", float_precision(v, s), v), r);
    }}

    parametrize_test! {test_pretty_duration, [
        (s: u64, n: u32, r: &'static str),
        (0, 0, "0s"),
        (0, 234_000_000, "0.23s"),
        (0, 236_000_000, "0.24s"),
        (1, 0, "1.0s"),
        (1, 100_000_000, "1.1s"),
        (5, 100_000_000, "5.1s"),
        (5, 260_000_000, "5.3s"),
        (50, 100_000, "50s"),
        (60, 0, "1m"),
        (60, 100_000, "1m"),
        (120, 0, "2m"),
        (3600, 0, "1h"),
        (3660, 0, "1h 1m"),
        (86460, 0, "1d 1m"),
        (93780, 0, "1d 2h 3m"),
        (93784, 0, "1d 2h 3m 4s"),
    ], {
        let duration = Duration::new(s, n);
        assert_eq!(format!("{}", PrettyDuration(duration)), r);
    }}
}
