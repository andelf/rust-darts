# rust-darts: Double-ARray Trie System Rust implementation.

Working in progress, USE AT YOUR RISK. PRs are welcomed.

A double array trie implementation. A Forward Maximum Matching Searcher is also provided.

The test dict files are borrowed from [hankcs/HanLP](https://github.com/hankcs/HanLP).

## Reference

- hankcs/HanLP: https://github.com/hankcs/HanLP
- Aho Corasick自动机结合DoubleArrayTrie极速多模式匹配: http://www.hankcs.com/program/algorithm/aho-corasick-double-array-trie.html
- DoubleArrayTrie和AhoCorasickDoubleArrayTrie的实用性对比: http://www.hankcs.com/program/algorithm/double-array-trie-vs-aho-corasick-double-array-trie.html
- Darts: Double-ARray Trie System: http://chasen.org/~taku/software/darts/
- An Implementation of Double-Array Trie: https://linux.thai.net/~thep/datrie/datrie.html

## Misc

Init dictionary(for following testcases):

```bash
# need minutes, with progress indicator
time cargo test -- --nocapture --ignored test_dat_basic
```

Running bench:

```bash
cargo bench -- --nocapture bench_dat_searcher
```
