# rust-darts: Double-Array Trie Rust implementation.

This library is in alpha state, PRs are welcomed. An optional Forward Maximum Matching Searcher is provided when enabled by features.

[![Build Status](https://travis-ci.org/andelf/rust-darts.svg?branch=master)](https://travis-ci.org/andelf/rust-darts)
[![codecov](https://codecov.io/gh/andelf/rust-darts/branch/master/graph/badge.svg)](https://codecov.io/gh/andelf/rust-darts)
[![Crates.io](https://img.shields.io/crates/v/darts.svg)](https://crates.io/crates/darts)
[![docs.rs](https://docs.rs/darts/badge.svg)](https://docs.rs/darts/)

## Installation

Add it to your `Cargo.toml`:

 ```toml
 [dependencies]
 darts = "0.1"
 ```

then you are good to go. If you are using Rust 2015 you have to `extern crate darts` to your crate root as well.

## Example

```rust
use std::fs::File;
use darts::DoubleArrayTrie;

fn main() {
    let mut f = File::open("./priv/dict.big.bincode").unwrap();
    let da = DoubleArrayTrie::load(&mut f).unwrap();
    let string = "中华人民共和国";
    let prefixes = da.common_prefix_search(string).map(|matches| {
        matches
            .iter()
            .map(|(end_idx, v)| {
                &string[..end_idx]
            })
            .collect();
    }).unwrap_or(vec![]);
    assert_eq!(vec!["中", "中华", "中华人民", "中华人民共和国"], prefixes);
}
```

```rust
use std::fs::File;
use darts::DoubleArrayTrie;

fn main() {
    let mut f = File::open("./priv/dict.big.bincode").unwrap();
    let da = DoubleArrayTrie::load(&mut f).unwrap();
    assert!(da.exact_match_search("东湖高新技术开发区").is_some());
}
```

## Enabling Additional Features

* `searcher` feature enables searcher for maximum forward matcher
* `serialization` feature enables saving and loading serialized `DoubleArrayTrie` data

```toml
[dependencies]
darts = { version = "0.1", features = ["searcher", "serialization"] }
```

## To Rebuild Dictionary

```bash
# It would take minutes, be patient.
time cargo test -- --nocapture --ignored test_dat_basic
```

## To run benchmark tests
```bash
cargo bench -- --nocapture
```

## License

This work is released under the MIT license. A copy of the license is provided in the LICENSE file.

## Reference

- [hankcs/HanLP](https://github.com/hankcs/HanLP)
- [Aho Corasick自动机结合DoubleArrayTrie极速多模式匹配](http://www.hankcs.com/program/algorithm/aho-corasick-double-array-trie.html)
- [DoubleArrayTrie和AhoCorasickDoubleArrayTrie的实用性对比](http://www.hankcs.com/program/algorithm/double-array-trie-vs-aho-corasick-double-array-trie.html)
- [Darts: Double-Array Trie System](http://chasen.org/~taku/software/darts/)
- [An Implementation of Double-Array Trie](https://linux.thai.net/~thep/datrie/datrie.html)
