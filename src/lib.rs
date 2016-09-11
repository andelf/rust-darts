#![feature(test)]
extern crate test;

use std::str;
use std::iter;
use std::vec;


// rust String is stored as Vec<u8> instead of [char] like Java.
// indexing rust String by chars uses linear scan

#[inline]
fn max<T: PartialOrd + Copy>(a: T, b: T) -> T {
    if a > b { a } else { b }
}



struct Node {
    code: usize,
    depth: usize,
    left: usize,
    right: usize
}


pub struct DoubleArrayTrieBuilder<'a> {
    check: Vec<u32>,
    base: Vec<i32>,
    used: Vec<bool>,

    size: usize,
    alloc_size: usize,
    keys: Vec<str::Chars<'a>>,   // String::chars() iterator
    finished: Vec<bool>,
    next_check_pos: usize,


    progress: usize,
    progress_func: Option<Box<Fn(usize,usize)->()>>
}

impl<'a> DoubleArrayTrieBuilder<'a>  {
    pub fn new() -> DoubleArrayTrieBuilder<'a> {
        DoubleArrayTrieBuilder {
            check: vec![],
            base: vec![],
            used: vec![],

            size: 0,
            alloc_size: 0,
            keys: vec![],
            finished: vec![],

            next_check_pos: 0,
            progress: 0,
            progress_func: None
        }
    }

    pub fn progress<F>(mut self, func: F) -> DoubleArrayTrieBuilder<'a>
        where F: 'static + Fn(usize, usize) -> () {

        self.progress_func = Some(Box::new(func));
        self
    }

    // pub fn build(mut self, keys: &[&str], values: &[usize]) -> DoubleArrayTrie {
    pub fn build(mut self, keys: &'a [&str]) -> DoubleArrayTrie {

        // must be size of single store
        self.resize(std::char::MAX as usize);

        self.keys = keys.iter()
            .map(|s| s.chars())
            .collect();
        self.finished.resize(keys.len(), false);

        self.base[0] = 1;
        self.next_check_pos = 0;

        let root_node = Node {
            code: 0,
            left: 0,
            right: keys.len(),
            depth: 0
        };

        let mut siblings = Vec::new();
        self.fetch(&root_node, &mut siblings);
        self.insert(&siblings);

        let DoubleArrayTrieBuilder { check, base, .. } = self;

        DoubleArrayTrie {
            check: check,
            base: base
        }
    }

    fn resize(&mut self, new_len: usize) {
        self.check.resize(new_len, 0);
        self.base.resize(new_len, 0);
        self.used.resize(new_len, false);

        self.alloc_size = new_len;
    }

    fn fetch(&mut self, parent: &Node, siblings: &mut Vec<Node>) -> usize {

        let mut prev = 0;

        //println!("left / right => ({}, {})", parent.left, parent.right);
        for i in parent.left .. parent.right {
            if self.finished[i] {
                continue;
            }
            let curr = self.keys[i]
                .next()
                .map_or(0, |c| c as usize + 1);

            assert!(prev <= curr, "keys must be sorted!");

            if curr == 0 {
                self.finished[i] = true;
            }
            if curr != prev || siblings.len() == 0 {
                let tmp_node = Node {
                    code: curr,
                    depth: parent.depth + 1,
                    left: i,
                    right: 0
                };

                siblings.last_mut().map(|n| n.right = i);
                siblings.push(tmp_node);
            }
            prev = curr;
        }

        siblings.last_mut().map(|n| n.right = parent.right);
        siblings.len()
    }

    fn insert(&mut self, siblings: &[Node]) -> usize {

        let mut begin: usize;
        let mut pos = max(siblings[0].code + 1, self.next_check_pos) - 1;
        let mut nonzero_num = 0;
        let mut first = 0;
        let key_size = self.keys.len();

        if self.alloc_size <= pos {
            self.resize(pos + 1);
        }

        'outer: loop {
            pos += 1;

            if self.alloc_size <= pos {
                self.resize(pos + 1);
            }

            if self.check[pos] != 0 {
                nonzero_num += 1;
                continue;
            } else if first == 0 {
                self.next_check_pos = pos;
                first = 1;
            }

            begin = pos - siblings[0].code;
            if self.alloc_size <= begin + siblings.last().map(|n| n.code).unwrap() {
                let l = (self.alloc_size as f32) *
                    max(1.05, key_size as f32 / (self.progress as f32 + 1.0));
                self.resize(l as usize)
            }

            if self.used[begin] {
                continue;
            }

            for n in siblings.iter() {
                if self.check[begin + n.code] != 0 {
                    continue 'outer;
                }
            }

            break;
        }

        // Simple heuristics
        // 从位置 next_check_pos 开始到 pos 间，如果已占用的空间在95%以上，
        // 下次插入节点时，直接从 pos 位置处开始查找
        if nonzero_num as f32 / (pos as f32 - self.next_check_pos as f32 + 1.0) >= 0.95 {
            self.next_check_pos = pos;
        }

        self.used[begin] = true;
        self.size = max(self.size, begin + siblings.last().map(|n| n.code).unwrap() + 1);

        siblings.iter().map(|n| self.check[begin + n.code] = begin as u32).last();

        for sibling in siblings.iter() {
            let mut new_siblings = Vec::new();

            // 一个词的终止且不为其他词的前缀，其实就是叶子节点
            if self.fetch(&sibling, &mut new_siblings) == 0 {
                // FIXME: ignore value ***
                self.base[begin + sibling.code] = - (sibling.left as i32) - 1;
                self.progress += 1;
                self.progress_func.as_ref().map(|f| f(self.progress, key_size));

            } else {
                let h = self.insert(&new_siblings);
                self.base[begin + sibling.code] = h as i32;
            }
        }

        begin
    }
}


#[derive(Debug)]
pub struct DoubleArrayTrie {
    base: Vec<i32>,             // use negetive to indicate fail
    check: Vec<u32>,
}


impl DoubleArrayTrie {
    pub fn exact_match_search(&self, key: &str) -> Option<usize> {

        let mut b = self.base[0];
        let mut p: usize;

        for c in key.chars() {
            // println!("matching s[{}] = {}", i, c);
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
            Some((-n-1) as usize)
        } else {
            None
        }
    }
}




#[cfg(test)]
mod tests {
    use super::*;
    use std::io::prelude::*;
    use std::io::BufReader;
    use std::fs::File;

    use test::Bencher;


    #[test]
    fn test_double_array_trie() {
        let f = File::open("./priv/dict.txt.big").unwrap();

        let mut keys: Vec<String> = BufReader::new(f)
            .lines()
            .map(|s| s.unwrap())
            .collect();

        keys.sort();

        let strs: Vec<&str> = keys.iter()
            .map(|n| n.split(' ').next().unwrap())
            .collect();

        let da = DoubleArrayTrieBuilder::new()
        //    .progress(|c, t| println!("{}/{}", c, t))
            .build(&strs);

        println!("find => {:?}", da.exact_match_search("she"));
        println!("find => {:?}", da.exact_match_search("万能胶"));
        println!("find => {:?}", da.exact_match_search("呼伦贝尔"));
        println!("find => {:?}", da.exact_match_search("东湖高新技术开发区"));
    }

    #[bench]
    fn bench_double_array_trie_build(b: &mut Bencher) {

        let f = File::open("./priv/dict.txt.big").unwrap();

        let keys: Vec<String> = BufReader::new(f)
            .lines()
            .map(|s| s.unwrap())
            .collect();

        let strs: Vec<&str> = keys.iter()
            .map(|n| n.split(' ').next().unwrap())
            .collect();

        b.iter(|| DoubleArrayTrieBuilder::new().build(&strs));
    }


    #[bench]
    fn bench_double_array_trie_match(b: &mut Bencher) {
        let f = File::open("./priv/dict.txt.big").unwrap();

        let keys: Vec<String> = BufReader::new(f)
            .lines()
            .map(|s| s.unwrap())
            .collect();

        let strs: Vec<&str> = keys.iter()
            .map(|n| n.split(' ').next().unwrap())
            .collect();

        let da = DoubleArrayTrieBuilder::new().build(&strs);
        b.iter(|| da.exact_match_search("东湖高新技术开发区"));
    }

}

/*
// bench using u8
./priv/dict.txt.big
test tests::bench_double_array_trie_build ... bench: 485,625,473 ns/iter (+/- 47,894,684)
test tests::bench_double_array_trie_match ... bench:          71 ns/iter (+/- 9)

// bench using char

./priv/dict.txt.big
test tests::bench_double_array_trie_build ... bench: 3,982,326,218 ns/iter (+/- 390,757,451)
test tests::bench_double_array_trie_match ... bench:          33 ns/iter (+/- 12)


cargo test -- --nocapture test  429.08s user 3.26s system 290% cpu 2:28.75 total

without print
cargo test -- --nocapture test  410.55s user 0.80s system 296% cpu 2:18.80 total
*/
