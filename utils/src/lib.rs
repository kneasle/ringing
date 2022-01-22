//! A set of useful utilities which are used by my ringing projects but aren't directly related to
//! ringing.  The name `kneasle_ringing_utils` is intentionally obscure; this isn't meant for
//! public use but needs to be on crates.io so that Monument can be installed with `cargo install`.
//! I don't want to clutter up possibly useful names on crates.io, hence the obscure name.

use std::{
    fmt::{Display, Formatter},
    time::Duration,
};

use number_prefix::NumberPrefix;

/// A wrapper around [`Duration`] which formats in a way that's easy to digest for all values,
/// whether that's nanoseconds or many days.
///
/// # Example
/// ```
/// use std::time::Duration;
/// use kneasle_ringing_utils::PrettyDuration;
///
/// // Really short time
/// assert_eq!(
///     format!("{}", PrettyDuration(Duration::from_secs_f32(0.00_00123451))),
///     "12.35µs"
/// );
/// assert_eq!(
///     format!("{}", PrettyDuration(Duration::from_secs_f32(10234.0))),
///     "2h 50m 34.00s"
/// );
/// ```
#[derive(Clone, Copy)]
pub struct PrettyDuration(pub Duration);

impl Display for PrettyDuration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let total_secs = self.0.as_secs_f64();
        if self.0 == Duration::ZERO {
            write!(f, "0.00s") // `Duration` gives this "0.00ns", which IMO is confusing
        } else if total_secs < 60.0 {
            // If the Duration is shorter than a minute, then using duration's 'debug' output to 2
            // decimal places does a pretty nice job
            write!(f, "{:.2?}", self.0)
        } else {
            // If the Duration is longer than a minute, we split it into days, hours, mins and (some
            // decimal number of) seconds like `#d #h #m #.#s`.  Again, the seconds goes to 2 decimal
            // places

            // Split out the (integer) number of minutes
            let total_mins = (total_secs / 60.0).floor() as usize;
            let num_secs = total_secs - total_mins as f64 * 60.0;
            // Split out the (integer) number of hours
            let total_hours = total_mins / 60;
            let num_mins = total_mins % 60;
            // Split out the (integer) number of days
            let total_days = total_hours / 24;
            let num_hours = total_hours % 24;

            // Generate the format string
            if total_days > 0 {
                write!(f, "{}d ", total_days)?;
            }
            if total_hours > 0 {
                write!(f, "{}h ", num_hours)?;
            }
            if total_mins > 0 {
                write!(f, "{}m ", num_mins)?;
            }
            write!(f, "{:.2}s", num_secs)
        }
    }
}

/// A wrapper over integers where [`Display`] formats the number suffixed with a multiplier
/// (e.g. `M` or `G` for millions or billions).  This differs from [`BigNumFloat`] because it will
/// never output a decimal number of values.
#[derive(Clone, Copy)]
pub struct BigNumInt(pub usize);

impl Display for BigNumInt {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match NumberPrefix::decimal(self.0 as f64) {
            NumberPrefix::Standalone(n) => write!(f, "{:.0}", n),
            NumberPrefix::Prefixed(prefix, n) => write!(f, "{:.2}{}", n, prefix),
        }
    }
}

/// A wrapper over floats where [`Display`] formats the number suffixed with a multiplier
/// (e.g. `M` or `G` for millions or billions).  This differs from [`BigNumInt`] because it will
/// output up to 3 decimal places if the number is small enough
pub struct BigNumFloat(pub f64);

impl Display for BigNumFloat {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match NumberPrefix::decimal(self.0) {
            NumberPrefix::Standalone(n) => write!(f, "{:.1}", n),
            NumberPrefix::Prefixed(prefix, n) => write!(f, "{:.2}{}", n, prefix),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_duration() {
        #[track_caller]
        fn check(secs: f64, output: &str) {
            let dur = Duration::from_secs_f64(secs);
            assert_eq!(format!("{}", PrettyDuration(dur)), output);
        }

        check(0.0, "0.00s");
        check(0.000_000_031_24156, "31.00ns"); // `Duration` is only accurate to the nearest nanosecond
        check(0.000_312_4156, "312.42µs");
        check(0.001_23015456, "1.23ms");
        check(5.2321345, "5.23s");
        check(60.0, "1m 0.00s");
        check(60.0005, "1m 0.00s");
        check(60.5, "1m 0.50s");
        check(3600.0, "1h 0m 0.00s");
        check(3600.5, "1h 0m 0.50s");
        check(8247.15213, "2h 17m 27.15s");
        check(18247.15213, "5h 4m 7.15s");
        check(118247.15213, "1d 8h 50m 47.15s");
    }

    #[test]
    fn big_num_int() {
        #[track_caller]
        fn check(v: usize, output: &str) {
            assert_eq!(format!("{}", BigNumInt(v)), output);
        }

        check(0, "0");
        check(102, "102");
        check(1_234, "1.23k");
        check(12_357_123, "12.36M");
        check(1_312_357_123, "1.31G");
        check(56_312_357_123, "56.31G");
        check(1_456_312_357_123, "1.46T");
    }

    #[test]
    fn big_num_float() {
        #[track_caller]
        fn check(v: f64, output: &str) {
            assert_eq!(format!("{}", BigNumFloat(v)), output);
        }

        check(-100.0, "-100.0");
        check(0.0, "0.0");
        check(0.001_35, "0.0"); // Small numbers don't get prefixes
        check(102.0, "102.0");
        check(1_234.0, "1.23k");
        check(12_357_123.0, "12.36M");
        check(1_312_357_123.0, "1.31G");
        check(56_312_357_123.0, "56.31G");
        check(1_456_312_357_123.0, "1.46T");
    }
}
