/// A class of characters, used for characterizing a run of text.
#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub enum CharKind {
    /// Whitespace.
    Whitespace,
    /// Punctuation.
    Punctuation,
    /// Word.
    Word,
}

#[derive(Default, Debug)]
pub struct CharClassifier {
    for_completion: bool,
    ignore_punctuation: bool,
}

impl CharClassifier {
    pub fn new() -> Self {
        Self {
            for_completion: false,
            ignore_punctuation: false,
        }
    }

    pub fn for_completion(self, for_completion: bool) -> Self {
        Self {
            for_completion,
            ..self
        }
    }

    pub fn ignore_punctuation(self, ignore_punctuation: bool) -> Self {
        Self {
            ignore_punctuation,
            ..self
        }
    }

    pub fn is_whitespace(&self, c: char) -> bool {
        self.kind(c) == CharKind::Whitespace
    }

    pub fn is_word(&self, c: char) -> bool {
        self.kind(c) == CharKind::Word
    }

    pub fn is_punctuation(&self, c: char) -> bool {
        self.kind(c) == CharKind::Punctuation
    }

    pub fn kind_with(&self, c: char, ignore_punctuation: bool) -> CharKind {
        if c.is_whitespace() {
            return CharKind::Whitespace;
        } else if c.is_alphanumeric() || c == '_' {
            return CharKind::Word;
        }

        // if let Some(scope) = &self.scope {
        //     if let Some(characters) = scope.word_characters() {
        //         if characters.contains(&c) {
        //             if c == '-' && !self.for_completion && !ignore_punctuation {
        //                 return CharKind::Punctuation;
        //             }
        //             return CharKind::Word;
        //         }
        //     }
        // }

        if ignore_punctuation {
            CharKind::Word
        } else {
            CharKind::Punctuation
        }
    }

    pub fn kind(&self, c: char) -> CharKind {
        self.kind_with(c, self.ignore_punctuation)
    }
}
