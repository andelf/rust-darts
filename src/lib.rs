//! Double Array Trie in Rust
//!
//! ## Installation
//!
//! Add it to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! darts = "0.1"
//! ```
//!
//! Then you are good to go. If you are using Rust 2015 you have to ``extern crate darts`` to your crate root as well.
//!
//! ## Example
//!
//! ```rust
//! use std::fs::File;
//! use darts::DoubleArrayTrie;
//!
//! fn main() {
//!     let mut f = File::open("./priv/dict.big.bincode").unwrap();
//!     let da = DoubleArrayTrie::load(&mut f).unwrap();
//!     let string = "中华人民共和国";
//!     let prefixes = da.common_prefix_search(string).map(|matches| {
//!         matches
//!             .into_iter()
//!             .map(|(end_idx, v)| {
//!                 &string[..end_idx]
//!             })
//!             .collect()
//!     }).unwrap_or(vec![]);
//!     assert_eq!(vec!["中", "中华", "中华人民", "中华人民共和国"], prefixes);
//! }
//! ```
//!
//! ```rust
//! use std::fs::File;
//! use darts::DoubleArrayTrie;
//!
//! fn main() {
//!     let mut f = File::open("./priv/dict.big.bincode").unwrap();
//!     let da = DoubleArrayTrie::load(&mut f).unwrap();
//!     assert!(da.exact_match_search("东湖高新技术开发区").is_some());
//! }
//! ```
//!
//! ## Enabling Additional Features
//!
//! * `searcher` feature enables searcher for maximum forward matcher
//!
//! ```toml
//! [dependencies]
//! darts = { version = "0.1", features = ["searcher"] }
//! ```
//!

#![cfg_attr(feature = "dev", plugin(clippy))]

extern crate bincode;
extern crate serde;

#[cfg(any(feature = "searcher"))]
pub mod searcher;

use std::cmp;
use std::error;
use std::fmt;
use std::io;
use std::io::prelude::*;
use std::iter;
use std::result;
use std::str;
use std::vec;

use serde::{Deserialize, Serialize};

/// The error type which is used in this crate.
#[derive(Debug)]
pub enum DartsError {
    Serialize(Box<bincode::ErrorKind>),
    Io(io::Error),
}

impl fmt::Display for DartsError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "rust-darts error")
    }
}

impl error::Error for DartsError {
    fn description(&self) -> &str {
        match *self {
            DartsError::Serialize(ref err) => err.description(),
            DartsError::Io(ref err) => err.description(),
        }
    }
}

/// The result type which is used in this crate.
pub type Result<T> = result::Result<T, DartsError>;

impl From<io::Error> for DartsError {
    fn from(err: io::Error) -> Self {
        DartsError::Io(err)
    }
}

impl From<Box<bincode::ErrorKind>> for DartsError {
    fn from(err: Box<bincode::ErrorKind>) -> Self {
        DartsError::Serialize(err)
    }
}

struct Node {
    code: usize,
    depth: usize,
    left: usize,
    right: usize,
}

/// Build a Double Arrary Trie from a series of strings.
pub struct DoubleArrayTrieBuilder<'a> {
    check: Vec<u32>,
    base: Vec<i32>,
    used: Vec<bool>,

    size: usize,
    alloc_size: usize,
    keys: Vec<iter::Chain<str::Chars<'a>, vec::IntoIter<char>>>, // String::chars() iterator
    next_check_pos: usize,

    progress: usize,
    progress_func: Option<Box<dyn Fn(usize, usize) -> ()>>,
}

#[allow(clippy::new_without_default)]
impl<'a> DoubleArrayTrieBuilder<'a> {
    pub fn new() -> DoubleArrayTrieBuilder<'a> {
        DoubleArrayTrieBuilder {
            check: vec![],
            base: vec![],
            used: vec![],
            size: 0,
            alloc_size: 0,
            keys: vec![],
            next_check_pos: 0,
            progress: 0,
            progress_func: None,
        }
    }

    /// Set callback to inspect trie building progress.
    pub fn progress<F>(mut self, func: F) -> DoubleArrayTrieBuilder<'a>
    where
        F: 'static + Fn(usize, usize) -> (),
    {
        self.progress_func = Some(Box::new(func));
        self
    }

    /// Start the building process from root layer, and recursively calling `fetch` and `insert` to
    /// construct the arrays
    pub fn build(mut self, keys: &'a [&str]) -> DoubleArrayTrie {
        // using the unicode scalar len is correct since that's our DARTS unit here
        let longest_word_len = keys.iter().map(|s| s.chars().count()).max().unwrap_or(0);

        // it should be at least the range of unicode scalar size since we are offseting by `code`
        self.resize(std::char::MAX as usize);

        self.keys = keys.iter().map(|s| s.chars().chain(vec!['\u{0}'])).collect();

        self.base[0] = 1;
        self.next_check_pos = 0;

        let root_node = Node {
            code: 0,
            left: 0,
            right: keys.len(),
            depth: 0,
        };

        let mut siblings = Vec::new();
        self.fetch(&root_node, &mut siblings);
        self.insert(&siblings);

        // shrink size, free the unnecessary memory
        let last_used_pos = self
            .used
            .iter()
            .enumerate()
            .rev()
            .find(|&(_, &k)| k)
            .map_or(self.alloc_size, |t| t.0 + std::char::MAX as usize);
        self.resize(last_used_pos);

        let DoubleArrayTrieBuilder { check, base, .. } = self;
        DoubleArrayTrie {
            check,
            base,
            longest_word_len,
        }
    }

    /// Resize all of the arrays we need
    fn resize(&mut self, new_len: usize) {
        self.check.resize(new_len, 0);
        self.base.resize(new_len, 0);
        self.used.resize(new_len, false);

        self.alloc_size = new_len;
    }

    /// To collect the children of `parent` node, by iterating through the same offset of the
    /// `keys`, and save the result into `siblings`, returning the number of siblings it collects.
    fn fetch(&mut self, parent: &Node, siblings: &mut Vec<Node>) -> usize {
        let mut prev = 0;

        // iterate over the same offset of the `keys`
        for i in parent.left..parent.right {
            let c = self.keys[i].next();

            if c.is_none() {
                continue;
            }

            let curr = c.map_or(0, |c| {
                if c != '\u{0}' {
                    c as usize + 1 // since we use \u{0} to indicate the termination of the string, every code has to be offset by 1
                } else {
                    0 // \u{0} as the termination of the string
                }
            });

            assert!(prev <= curr, "keys must be sorted!");

            // we found the adjacent characters in the same offset are different, that means we
            // should add one more sibling in the trie.
            if curr != prev || siblings.is_empty() {
                let tmp_node = Node {
                    code: curr,
                    depth: parent.depth + 1,
                    left: i,
                    right: 0,
                };
                if let Some(n) = siblings.last_mut() {
                    n.right = i;
                }
                siblings.push(tmp_node);
            }

            prev = curr;
        }

        if let Some(n) = siblings.last_mut() {
            n.right = parent.right;
        }
        siblings.len()
    }

    /// Insert the nodes in the `siblings` into `check` and `base`, returning the index where the
    /// `siblings` is inserted.
    fn insert(&mut self, siblings: &[Node]) -> usize {
        assert!(!siblings.is_empty());

        let mut begin: usize;
        let mut pos = cmp::max(siblings[0].code + 1, self.next_check_pos) - 1;
        let mut nonzero_num = 0; // the number of slots in check that already been taken
        let mut first = 0; // the flag to mark if we have run into the first time for the condition of "check[pos] == 0"
        let key_size = self.keys.len();

        if self.alloc_size <= pos {
            self.resize(pos + 1);
        }

        'outer: loop {
            pos += 1;

            if self.alloc_size <= pos {
                self.resize(pos + 1);
            }

            // iterate through the slot that already has an owner
            if self.check[pos] != 0 {
                nonzero_num += 1;
                continue;
            } else if first == 0 {
                self.next_check_pos = pos; // remember the slot so the next time we call `insert` we could save some time for searching
                first = 1;
            }

            // derive the `begin` in reverse, substract the code from `pos`
            begin = pos - siblings[0].code;

            if self.alloc_size <= begin + siblings.last().map(|n| n.code).unwrap() {
                let l = self.alloc_size * cmp::max(105, (key_size * 100) / (self.progress + 1)) / 100;
                self.resize(l as usize)
            }

            // then we check if the `begin` is already taken
            if self.used[begin] {
                continue;
            }

            // check if any of the slots where we should put the code are taken.
            for n in siblings.iter() {
                if self.check[begin + n.code] != 0 {
                    continue 'outer;
                }
            }

            // all are available, break out the loop.
            break;
        }

        // heuristic search, if the places we have iterated over where 95% of them are taken, then
        // we just jump start from `pos` in the next cycle
        if nonzero_num as f32 / (pos as f32 - self.next_check_pos as f32 + 1.0) >= 0.95 {
            self.next_check_pos = pos;
        }

        self.used[begin] = true;
        self.size = cmp::max(self.size, begin + siblings.last().map(|n| n.code).unwrap() + 1);

        // mark the ownership of these cells
        siblings
            .iter()
            .map(|n| self.check[begin + n.code] = begin as u32)
            .last();

        // recursively call `fetch` and `insert` for this level of the nodes
        for sibling in siblings.iter() {
            let mut new_siblings = Vec::new();

            // a string without any children, then it means we reach a leaf node.
            if self.fetch(sibling, &mut new_siblings) == 0 {
                // mark it as negative number to signal it is a leaf.
                self.base[begin + sibling.code] = -(sibling.left as i32) - 1;

                self.progress += 1;
                if let Some(f) = self.progress_func.as_ref() {
                    f(self.progress, key_size);
                }
            } else {
                let h = self.insert(&new_siblings);

                // save the insertion index into `base`
                self.base[begin + sibling.code] = h as i32;
            }
        }

        begin
    }
}

/// A Double Array Trie.
#[derive(Serialize, Deserialize, Debug)]
pub struct DoubleArrayTrie {
    base: Vec<i32>, // use negetive to indicate ends
    check: Vec<u32>,
    longest_word_len: usize,
}

impl DoubleArrayTrie {
    /// Match whole string.
    pub fn exact_match_search(&self, key: &str) -> Option<usize> {
        let mut b = self.base[0];
        let mut p: usize;

        for c in key.chars() {
            p = (b + c as i32 + 1) as usize;

            if b == self.check[p] as i32 {
                b = self.base[p];
            } else {
                return None;
            }
        }

        p = b as usize;
        let n = self.base[p];

        if b == self.check[p] as i32 && n < 0 {
            Some((-n - 1) as usize)
        } else {
            None
        }
    }

    /// Find all matched prefixes. Returns [(end_index, value)].
    pub fn common_prefix_search(&self, key: &str) -> Option<Vec<(usize, usize)>> {
        // Reserve the capacity to speed up the performance,
        // the longest prefix is at most the longest word in the dictionary,
        // and suppose each iteration matches a prefix,
        // there should be <longest_word_len> number of prefixes matched.
        let mut result = Vec::with_capacity(self.longest_word_len);

        let mut b = self.base[0];
        let mut n;
        let mut p: usize;

        for (i, c) in key.char_indices() {
            p = b as usize;
            n = self.base[p];

            if b == self.check[p] as i32 && n < 0 {
                result.push((i, (-n - 1) as usize))
            }

            p = b as usize + c as usize + 1;
            if b == self.check[p] as i32 {
                b = self.base[p];
            } else {
                return if result.is_empty() { None } else { Some(result) };
            }
        }

        p = b as usize;
        n = self.base[p];

        if b == self.check[p] as i32 && n < 0 {
            result.push((key.len(), (-n - 1) as usize));
            Some(result)
        } else {
            None
        }
    }

    /// Save DAT to an output stream.
    pub fn save<W: Write>(&self, w: &mut W) -> Result<()> {
        let encoded: Vec<u8> = bincode::serialize(self)?;
        w.write_all(&encoded).map_err(From::from)
    }

    /// Load DAT from input stream.
    pub fn load<R: Read>(r: &mut R) -> Result<Self> {
        let mut buf = Vec::new();
        r.read_to_end(&mut buf)?;
        Ok(bincode::deserialize(&buf)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::BufReader;

    #[test]
    #[ignore]
    fn test_dat_basic() {
        let f = File::open("./priv/dict.txt.big").unwrap();

        let mut keys: Vec<String> = BufReader::new(f).lines().map(|s| s.unwrap()).collect();

        // sort the key in lexigraphical order so that we don't need relocate the `base` and
        // `check`
        keys.sort();

        let strs: Vec<&str> = keys.iter().map(|n| n.split(' ').next().unwrap()).collect();

        let da = DoubleArrayTrieBuilder::new()
            .progress(|current, total| print!("\r{}% {}/{}", current * 100 / total, current, total))
            .build(&strs);

        println!("\nDone!");

        let _ = File::create("./priv/dict.big.bincode")
            .as_mut()
            .map(|f| da.save(f))
            .expect("write ok!");
    }

    #[test]
    fn test_dat_exact_match_search() {
        let mut f = File::open("./priv/dict.big.bincode").unwrap();
        let da = DoubleArrayTrie::load(&mut f).unwrap();

        let string = "中华人民共和国";
        da.common_prefix_search(string).as_ref().map(|matches| {
            matches
                .iter()
                .map(|&(end_idx, v)| {
                    println!("prefix[{}] = {}", &string[..end_idx], v);
                })
                .last()
        });
    }

    #[test]
    fn test_dat_prefix_search() {
        let mut f = File::open("./priv/dict.big.bincode").unwrap();
        let da = DoubleArrayTrie::load(&mut f).unwrap();
        assert!(da.exact_match_search("东湖高新技术开发区").is_some());
    }

    #[test]
    fn test_dat_builder() {
        let strs: Vec<&str> = vec!["a", "ab", "abc"];
        let da = DoubleArrayTrieBuilder::new().build(&strs);
        assert!(da.exact_match_search("abc").is_some());
    }
}
