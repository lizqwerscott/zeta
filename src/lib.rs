use std::ops::Range;

use emacs::{Env, Result, defun};

mod utils;

use utils::{CharClassifier, CharKind};

type DiffData = Vec<(Range<usize>, String)>;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "zeta-dyn", defun_prefix = "zeta")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

fn tokenize(text: &str) -> Vec<&str> {
    let classifier = CharClassifier::new().for_completion(true);
    let mut chars = text.chars().peekable();
    let mut prev_ch = chars.peek().copied();
    let mut tokens = Vec::new();
    let mut start = 0;
    let mut end = 0;
    while let Some(ch) = chars.next() {
        let prev_kind = prev_ch.map(|ch| classifier.kind(ch));
        let kind = classifier.kind(ch);
        if Some(kind) != prev_kind || (kind == CharKind::Punctuation && Some(ch) != prev_ch) {
            tokens.push(&text[start..end]);
            start = end;
        }
        end += ch.len_utf8();
        prev_ch = Some(ch);
    }
    tokens.push(&text[start..end]);
    tokens
}

fn diff_words_pos(text: String, alt_text: String, offset: u64) -> DiffData {
    // tokenize
    let old_tokens = tokenize(&text);
    let new_tokens = tokenize(&alt_text);

    // diff
    let diff = similar::TextDiffConfig::default()
        .algorithm(similar::Algorithm::Patience)
        .diff_slices(&old_tokens, &new_tokens);

    let mut edits: DiffData = Vec::new();
    let mut old_start = offset as usize;
    for change in diff.iter_all_changes() {
        let value = change.value();
        match change.tag() {
            similar::ChangeTag::Equal => {
                old_start += value.len();
            }
            similar::ChangeTag::Delete => {
                let old_end = old_start + value.len();
                if let Some((last_old_range, _)) = edits.last_mut() {
                    if last_old_range.end == old_start {
                        last_old_range.end = old_end;
                    } else {
                        edits.push((old_start..old_end, String::new()));
                    }
                } else {
                    edits.push((old_start..old_end, String::new()));
                }
                old_start = old_end;
            }
            similar::ChangeTag::Insert => {
                if let Some((last_old_range, last_new_text)) = edits.last_mut() {
                    if last_old_range.end == old_start {
                        last_new_text.push_str(value);
                    } else {
                        edits.push((old_start..old_start, value.into()));
                    }
                } else {
                    edits.push((old_start..old_start, value.into()));
                }
            }
        }
    }

    edits
}

#[defun(user_ptr)]
fn diff_words(env: &Env, text: String, alt_text: String, offset: u64) -> Result<DiffData> {
    let res = diff_words_pos(text, alt_text, offset);
    env.message("res")?;
    Ok(res)
}

#[defun]
fn diff_data_len(diff_data: &DiffData) -> Result<usize> {
    Ok(diff_data.len())
}

#[defun]
fn diff_data_start(diff_data: &DiffData, i: u64) -> Result<Option<usize>> {
    Ok(diff_data.get(i as usize).map(|data| data.0.start))
}

#[defun]
fn diff_data_end(diff_data: &DiffData, i: u64) -> Result<Option<usize>> {
    Ok(diff_data.get(i as usize).map(|data| data.0.end))
}

#[defun]
fn diff_data_data(diff_data: &DiffData, i: u64) -> Result<Option<String>> {
    Ok(diff_data.get(i as usize).map(|data| data.1.clone()))
}

#[cfg(test)]
mod lib_tests {
    use super::*;

    #[test]
    fn test_diff_words() {
        let old_text = String::from(
            "
    let pivot_index = partition(arrl);
    let a = 1;
    let b = 2;
    let

}
",
        );

        let new_text = String::from(
            "
    let pivot = partitio(arrr);
    let a = 1;
    let b = 2;
    let left = quick_sort(&arr[..pivot_index]);
    let right = quick_sort(&arr[pivot_index + 1..]);

}
",
        );

        let res = diff_words_pos(old_text, new_text, 0);
        println!("res: {:?}", res);
    }
}
