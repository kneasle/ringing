# kneasle_ringing_utils

A ['kitchen-sink'](https://matklad.github.io/2021/05/12/design-pattern-dumping-ground.html) crate,
containing various useful non-ringing-related utilities that are used by other crates but don't belong
in BellFrame.  This has an intentionally long crate name, so that I can publish it to
[crates.io](https://crates.io) (thus making `cargo install monument_cli` work) without worrying
about using up names that might be used for more useful crates.
