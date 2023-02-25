use rand::{rngs::ThreadRng, Rng};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Die {
    D4,
    D6,
    D8,
    D10,
    D12,
    D20,
    /// A d100 breaks the rules of most dice, in that it contains the values 0-99, as opposed to
    /// 1-100
    D100,
}

impl Die {
    pub fn roll(&self, rng: &mut ThreadRng) -> i64 {
        if let Die::D100 = self {
            rng.gen_range(0..100)
        } else {
            rng.gen_range(1..=self.number_of_sides())
        }
    }

    pub fn number_of_sides(&self) -> i64 {
        match self {
            Die::D4 => 4,
            Die::D6 => 6,
            Die::D8 => 8,
            Die::D10 => 10,
            Die::D12 => 12,
            Die::D20 => 20,
            Die::D100 => 100,
        }
    }
}

impl TryFrom<i64> for Die {
    type Error = ();

    fn try_from(value: i64) -> Result<Self, Self::Error> {
        match value {
            4 => Ok(Die::D4),
            6 => Ok(Die::D6),
            8 => Ok(Die::D8),
            10 => Ok(Die::D10),
            12 => Ok(Die::D12),
            20 => Ok(Die::D20),
            100 => Ok(Die::D100),
            _ => Err(()),
        }
    }
}
