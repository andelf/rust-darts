use std::fs::File;
use std::io::{self, BufRead, BufReader};
use darts::{DoubleArrayTrie, DoubleArrayTrieBuilder};

struct IndexBuilder {}

impl IndexBuilder {
    pub fn new() -> Self {
        IndexBuilder {}
    }

    // Require the dictionary to be sorted in lexicographical order
    pub fn build<R: BufRead>(&mut self, dict: &mut R) -> io::Result<DoubleArrayTrie> {
        let mut buf = String::new();
        let mut records: Vec<(String, usize, String)> = Vec::new();
        let mut prev_word = String::new();

        while dict.read_line(&mut buf)? > 0 {
            {
                let parts: Vec<&str> = buf.trim().split_whitespace().collect();
                if parts.is_empty() {
                    continue;
                }

                let word = parts[0];
                let freq = parts.get(1).map(|x| x.parse::<usize>().unwrap()).unwrap_or(0);
                let tag = parts.get(2).cloned().unwrap_or("");

                assert!(
                    (&*prev_word < word),
                    "the dictionary has to be sorted in lexicographical order."
                );
                prev_word = word.to_string();

                records.push((String::from(word), freq, String::from(tag)));
            }
            buf.clear();
        }

        let strs: Vec<&str> = records.iter().map(|n| n.0.as_ref()).collect();
        let da = DoubleArrayTrieBuilder::new().build(&strs);

        Ok(da)
    }
}

fn main() {
    let f = File::open("./dict.txt").unwrap();
    let mut buf = BufReader::new(f);
    let _ = IndexBuilder::new().build(&mut buf).unwrap();
}
