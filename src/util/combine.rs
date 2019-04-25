use combine::stream::state::Positioner;
use combine::stream::Resetable;

/// Represents a 0-based position.
#[derive(Default, Debug)]
pub(crate) struct OnelinePosition {
    pos: usize,
}

impl OnelinePosition {
    /// Constructs a new `OnelinePosition`.
    pub(crate) fn new() -> Self {
        Self::default()
    }
}

impl Positioner<char> for OnelinePosition {
    type Position = usize;

    fn position(&self) -> usize {
        self.pos
    }

    fn update(&mut self, _: &char) {
        self.pos += 1;
    }
}

impl Resetable for OnelinePosition {
    type Checkpoint = usize;

    fn checkpoint(&self) -> usize {
        self.pos
    }

    fn reset(&mut self, checkpoint: usize) {
        self.pos = checkpoint;
    }
}
