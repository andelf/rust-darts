#[macro_use]
extern crate criterion;
extern crate darts;
extern crate hashbrown;
extern crate lazy_static;

#[cfg(any(feature = "searcher"))]
use darts::searcher;

use criterion::Criterion;
use darts::DoubleArrayTrie;
use hashbrown::HashMap;
use lazy_static::lazy_static;
use std::fs::{read_to_string, File};
use std::io::{BufRead, BufReader};

lazy_static! {
    static ref DA: DoubleArrayTrie = {
        let mut f = File::open("./priv/dict.big.bincode").unwrap();
        DoubleArrayTrie::load(&mut f).unwrap()
    };
    static ref HASHMAP: HashMap<String, usize> = {
        let mut dict = HashMap::new();
        let file = File::open("./priv/dict.txt.big").unwrap();
        let mut rdr = BufReader::with_capacity(8 * (1 << 10), file);

        let mut buf = String::new();
        while rdr.read_line(&mut buf).unwrap() > 0 {
            {
                let parts: Vec<&str> = buf.trim().split_whitespace().collect();
                if parts.is_empty() {
                    // Skip empty lines
                    continue;
                }

                let word = parts[0];
                let freq = parts.get(1).map(|x| x.parse::<usize>().unwrap()).unwrap();

                dict.insert(String::from(word), freq);
            }
            buf.clear();
        }

        dict
    };
}

fn bench_dat_prefix_search() {
    DA.common_prefix_search("东湖高新技术开发区").unwrap();
}

fn bench_hashbrown_prefix_search() {
    let sentence: &str = "东湖高新技术开发区";
    let char_indices: Vec<usize> = sentence.char_indices().map(|x| x.0).collect();

    let word_count = char_indices.len();
    for (k, &byte_start) in char_indices.iter().enumerate() {
        let mut i = k;
        let wfrag = if k + 1 < char_indices.len() {
            &sentence[byte_start..char_indices[k + 1]]
        } else {
            &sentence[byte_start..]
        };

        while i < word_count {
            if HASHMAP.contains_key(wfrag) {
                //do nothing
            }

            i += 1;
        }
    }
}

fn bench_dat_match_found() {
    DA.exact_match_search(
        "我是拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上CEO，走上人生巅峰。",
    );
}

fn bench_hashbrown_match_found() {
    HASHMAP.contains_key(
        "我是拖拉机学院手扶拖拉机专业的。不用多久，我就会升职加薪，当上CEO，走上人生巅峰。",
    );
}

fn bench_dat_match_not_found_slow_fail() {
    DA.exact_match_search("东湖高新技术开发区abcdef");
}

fn bench_hashbrown_match_not_found_slow_fail() {
    HASHMAP.contains_key("东湖高新技术开发区abcdef");
}

fn bench_dat_match_not_found_fast_fail() {
    DA.exact_match_search("abcdef东湖高新技术开发区");
}

fn bench_hashbrown_match_not_found_fast_fail() {
    HASHMAP.contains_key("abcdef东湖高新技术开发区");
}

#[cfg(any(feature = "searcher"))]
fn bench_dat_searcher() {
    let text: String = read_to_string("./priv/weicheng.txt").unwrap();

    let mut searcher = DA.search(&text);
    loop {
        let step = searcher.next();
        if step == searcher::SearchStep::Done {
            break;
        }
    }
}

fn touch_static_variables() {
    DA.exact_match_search(" ");
    HASHMAP.contains_key(" ");
}

fn criterion_benchmark(c: &mut Criterion) {
    touch_static_variables();

    c.bench_function("dat prefix search", |b| b.iter(|| bench_dat_prefix_search()));

    c.bench_function("hashbrown prefix search", |b| {
        b.iter(|| bench_hashbrown_prefix_search())
    });

    c.bench_function("dat match found", |b| b.iter(|| bench_dat_match_found()));

    c.bench_function("hashbrown match found", |b| b.iter(|| bench_hashbrown_match_found()));

    c.bench_function("dat match not found fast fail", |b| {
        b.iter(|| bench_dat_match_not_found_fast_fail())
    });

    c.bench_function("hashbrown match not found fast fail", |b| {
        b.iter(|| bench_hashbrown_match_not_found_fast_fail())
    });

    c.bench_function("dat match not found slow fail", |b| {
        b.iter(|| bench_dat_match_not_found_slow_fail())
    });

    c.bench_function("hashbrown match not found slow fail", |b| {
        b.iter(|| bench_hashbrown_match_not_found_slow_fail())
    });

    #[cfg(any(feature = "searcher"))]
    c.bench_function("dat searcher", |b| b.iter(|| bench_dat_searcher()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
