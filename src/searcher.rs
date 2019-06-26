use crate::DoubleArrayTrie;

impl DoubleArrayTrie {
    /// Run Forward Maximum Matching Method on a string. Returns a Searcher iterator.
    pub fn search<'a, 'b>(&'b self, haystack: &'a str) -> DoubleArrayTrieSearcher<'a, 'b> {
        DoubleArrayTrieSearcher {
            haystack,
            dat: self,
            start_pos: 0,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SearchStep {
    Match(usize, usize),
    Reject(usize, usize),
}

/// A seracher for all words in Double Array Trie, using Forward Maximum Matching Method.
#[allow(dead_code)]
pub struct DoubleArrayTrieSearcher<'a, 'b> {
    haystack: &'a str,
    dat: &'b DoubleArrayTrie,
    start_pos: usize,
}

impl<'a, 'b> Iterator for DoubleArrayTrieSearcher<'a, 'b> {
    type Item = SearchStep;

    fn next(&mut self) -> Option<SearchStep> {
        let base = &self.dat.base;
        let check = &self.dat.check;

        let mut b = base[0];
        let mut n;
        let mut p: usize;

        let start_pos = self.start_pos;

        let mut next_pos = 0;
        let mut result = None;

        if start_pos >= self.haystack.len() {
            return None;
        }

        for (i, c) in self.haystack[start_pos..].char_indices() {
            p = b as usize;
            n = base[p];

            if b == check[p] as i32 && n < 0 {
                next_pos = start_pos + i;
                result = Some(SearchStep::Match(start_pos, start_pos + i));
            }

            p = b as usize + c as usize + 1;
            if b == check[p] as i32 {
                b = base[p];
            } else if result.is_some() {
                // last item is the maximum matching
                self.start_pos = next_pos;
                return result;
            } else {
                self.start_pos = start_pos + i + c.len_utf8();
                return Some(SearchStep::Reject(start_pos, self.start_pos));
            }
        }

        p = b as usize;
        n = base[p];

        // full match from start to end
        self.start_pos = self.haystack.len();
        if b == check[p] as i32 && n < 0 {
            Some(SearchStep::Match(start_pos, self.start_pos))
        } else {
            Some(SearchStep::Reject(start_pos, self.start_pos))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;

    fn search_step_to_str(step: &SearchStep, haystack: &str) -> String {
        match *step {
            SearchStep::Match(start, end) => format!("{}/n", &haystack[start..end]),
            SearchStep::Reject(start, end) => format!("{}/x", &haystack[start..end]),
        }
    }

    #[test]
    fn test_dat_searcher() {
        let mut f = File::open("./priv/dict.big.bincode").unwrap();
        let da = DoubleArrayTrie::load(&mut f).unwrap();

        let text = "江西鄱阳湖干枯，中国最大淡水湖变成大草原";
        let segmented = da
            .search(&text)
            .map(|s| search_step_to_str(&s, text))
            .collect::<Vec<String>>()
            .join(" ");
        assert_eq!(
            segmented,
            "江西/n 鄱阳湖/n 干枯/n ，/x 中国/n 最大/n 淡水湖/n 变成/n 大/n 草原/n"
        );
    }
}
