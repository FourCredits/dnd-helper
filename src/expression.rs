use std::{fmt, iter, str::FromStr};

use rand::rngs::ThreadRng;

use crate::die::Die;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expression {
    SingleDie(Die),
    MultipleDice(usize, Die),
    Constant(i64),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
}

impl Expression {
    pub fn evaluate(&self, rng: &mut ThreadRng) -> i64 {
        use Expression::*;
        match self {
            SingleDie(d) => d.roll(rng),
            MultipleDice(n, d) => iter::repeat(d).take(*n).map(|d| d.roll(rng)).sum(),
            Constant(n) => *n,
            Add(e1, e2) => e1.evaluate(rng) + e2.evaluate(rng),
            Subtract(e1, e2) => e1.evaluate(rng) - e2.evaluate(rng),
            Multiply(e1, e2) => e1.evaluate(rng) * e2.evaluate(rng),
        }
    }

    fn format_for_multiplication(&self) -> String {
        if matches!(self, Expression::Add(_, _) | Expression::Subtract(_, _)) {
            format!("({})", self)
        } else {
            format!("{}", self)
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expression::*;
        match self {
            SingleDie(d) => write!(f, "d{}", d.number_of_sides()),
            MultipleDice(n, d) => write!(f, "{}d{}", n, d.number_of_sides()),
            Constant(n) => write!(f, "{}", n),
            Add(left, right) => write!(f, "{} + {}", *left, *right),
            Subtract(left, right) => write!(f, "{} - {}", *left, *right),
            Multiply(left, right) => write!(
                f,
                "{} * {}",
                left.format_for_multiplication(),
                right.format_for_multiplication()
            ),
        }
    }
}

impl FromStr for Expression {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        whole_thing(parse_expression)(s).ok_or(())
    }
}

type ParseResult<'a, A> = Option<(A, &'a str)>;

fn parse_char<'a>(target: char) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    move |s| match s.chars().next() {
        Some(c) if c == target => Some((target, &s[1..])),
        _ => None,
    }
}

fn parse_natural_number(s: &str) -> ParseResult<u32> {
    let number_part: String = s.chars().take_while(char::is_ascii_digit).collect();
    if number_part.is_empty() {
        None
    } else {
        Some((number_part.parse().ok()?, &s[number_part.len()..]))
    }
}

fn parse_integer(s: &str) -> ParseResult<i64> {
    let (sign, s1) = match parse_char('-')(s) {
        Some((_, s1)) => (-1, s1),
        _ => (1, s),
    };
    let (magnitude, s2) = parse_natural_number(s1)?;
    Some((sign as i64 * magnitude as i64, s2))
}

fn parse_constant(s: &str) -> ParseResult<Expression> {
    parse_integer(s).map(|(i, s)| (Expression::Constant(i), s))
}

fn parse_die(s: &str) -> ParseResult<Die> {
    let (_, s1) = parse_char('d')(s)?;
    let (n, s2) = parse_natural_number(s1)?;
    Some(((n as i64).try_into().ok()?, s2))
}

fn parse_single_die(s: &str) -> ParseResult<Expression> {
    parse_die(s).map(|(die, s1)| (Expression::SingleDie(die), s1))
}

fn parse_multiple_dice(s: &str) -> ParseResult<Expression> {
    let (n, s1) = parse_natural_number(s)?;
    let (die, s2) = parse_die(s1)?;
    Some((Expression::MultipleDice(n as usize, die), s2))
}

fn or<'a, A>(
    p1: impl Fn(&'a str) -> ParseResult<'a, A>,
    p2: impl Fn(&'a str) -> ParseResult<'a, A>,
) -> impl Fn(&'a str) -> ParseResult<'a, A> {
    move |s| p1(s).or_else(|| p2(s))
}

fn parse_whitespace(s: &str) -> ParseResult<()> {
    let n = s.chars().take_while(|c| char::is_whitespace(*c)).count();
    (n > 0).then_some(((), &s[n..]))
}

fn optional<'a, A>(
    p: impl Fn(&'a str) -> ParseResult<'a, A>,
) -> impl Fn(&'a str) -> ParseResult<'a, Option<A>> {
    move |s| {
        p(s).map(|(value, s1)| (Some(value), s1))
            .or(Some((None, s)))
    }
}

fn operator(operation: char) -> impl Fn(&str) -> ParseResult<()> {
    move |s| {
        let (_, s1) = optional(parse_whitespace)(s)?;
        let (_, s2) = parse_char(operation)(s1)?;
        let (_, s3) = optional(parse_whitespace)(s2)?;
        Some(((), s3))
    }
}

fn parse_bracketed_expression(s: &str) -> ParseResult<Expression> {
    let (_, s1) = parse_char('(')(s)?;
    let (_, s2) = optional(parse_whitespace)(s1)?;
    let (expr, s3) = parse_expression(s2)?;
    let (_, s4) = optional(parse_whitespace)(s3)?;
    let (_, s5) = parse_char(')')(s4)?;
    Some((expr, s5))
}

fn parse_below_multiply(s: &str) -> ParseResult<Expression> {
    or(
        or(
            or(parse_bracketed_expression, parse_multiple_dice),
            parse_single_die,
        ),
        parse_constant,
    )(s)
}

fn parse_below_subtraction(s: &str) -> ParseResult<Expression> {
    or(parse_multiply_expression, parse_below_multiply)(s)
}

fn parse_below_addition(s: &str) -> ParseResult<Expression> {
    or(parse_subtraction_expression, parse_below_subtraction)(s)
}

fn parse_multiply_expression(s: &str) -> ParseResult<Expression> {
    let (left, s1) = parse_below_multiply(s)?;
    let (_, s2) = operator('*')(s1)?;
    let (right, s3) = parse_below_subtraction(s2)?;
    Some((Expression::Multiply(Box::new(left), Box::new(right)), s3))
}

fn parse_subtraction_expression(s: &str) -> ParseResult<Expression> {
    let (left, s1) = parse_below_subtraction(s)?;
    let (_, s2) = operator('-')(s1)?;
    let (right, s3) = parse_below_addition(s2)?;
    Some((Expression::Subtract(Box::new(left), Box::new(right)), s3))
}

fn parse_addition_expression(s: &str) -> ParseResult<Expression> {
    let (left, s1) = parse_below_addition(s)?;
    let (_, s2) = operator('+')(s1)?;
    let (right, s3) = parse_expression(s2)?;
    Some((Expression::Add(Box::new(left), Box::new(right)), s3))
}

fn whole_thing<A>(p: impl Fn(&str) -> ParseResult<A>) -> impl Fn(&str) -> Option<A> {
    move |s| p(s).and_then(|(value, leftover)| leftover.is_empty().then_some(value))
}

fn parse_expression(s: &str) -> ParseResult<Expression> {
    or(
        or(parse_addition_expression, parse_subtraction_expression),
        parse_below_addition,
    )(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod natural_number {
        use super::super::*;

        #[test]
        fn base_case() {
            assert_eq!(parse_natural_number("543"), Some((543, "")));
        }

        #[test]
        fn random_stuff() {
            assert_eq!(parse_natural_number("asb54"), None);
        }

        #[test]
        fn beginning_with_number() {
            assert_eq!(parse_natural_number("54asb"), Some((54, "asb")));
        }

        #[test]
        fn negative_numbers_dont_work() {
            assert_eq!(parse_natural_number("-45"), None);
        }
    }

    mod integer {
        use super::super::*;

        #[test]
        fn missing_sign() {
            assert_eq!(parse_integer("54"), Some((54, "")));
        }

        #[test]
        fn with_sign() {
            assert_eq!(parse_integer("-54"), Some((-54, "")));
        }

        #[test]
        fn sign_must_be_at_start() {
            assert_eq!(parse_integer("5-4"), Some((5, "-4")));
        }
    }

    mod die {
        use super::super::*;
        use crate::die::Die::*;

        #[test]
        fn base_cases() {
            assert_eq!(parse_die("d4"), Some((D4, "")));
            assert_eq!(parse_die("d6"), Some((D6, "")));
            assert_eq!(parse_die("d8"), Some((D8, "")));
            assert_eq!(parse_die("d10"), Some((D10, "")));
            assert_eq!(parse_die("d12"), Some((D12, "")));
            assert_eq!(parse_die("d20"), Some((D20, "")));
            assert_eq!(parse_die("d100"), Some((D100, "")));
        }

        #[test]
        fn text_afterwards() {
            assert_eq!(parse_die("d6ba"), Some((D6, "ba")));
        }
    }

    #[test]
    fn multiple_dice() {
        assert_eq!(
            parse_multiple_dice("654d100"),
            Some((Expression::MultipleDice(654, Die::D100), ""))
        )
    }

    mod multiplication {
        use super::super::*;
        use crate::die::Die::*;
        use crate::expression::Expression::*;

        #[test]
        fn two_constants() {
            assert_eq!(
                parse_multiply_expression("54 * 98"),
                Some((Multiply(Box::new(Constant(54)), Box::new(Constant(98))), ""))
            );
        }

        #[test]
        fn multiple_dice_and_constant() {
            assert_eq!(
                parse_multiply_expression("4d4 * 98"),
                Some((
                    Multiply(Box::new(MultipleDice(4, D4)), Box::new(Constant(98))),
                    ""
                ))
            );
        }

        #[test]
        fn whitespace_is_irrelevant() {
            let result = Multiply(Box::new(Constant(5)), Box::new(Constant(6)));
            assert_eq!(
                parse_multiply_expression("5 * 6"),
                Some((result.clone(), ""))
            );
            assert_eq!(
                parse_multiply_expression("5 *6"),
                Some((result.clone(), ""))
            );
            assert_eq!(
                parse_multiply_expression("5* 6"),
                Some((result.clone(), ""))
            );
            assert_eq!(parse_multiply_expression("5*6"), Some((result, "")));
        }
    }

    mod addition {
        use super::super::*;
        use crate::die::Die::*;
        use crate::expression::Expression::*;

        #[test]
        fn two_constants() {
            assert_eq!(
                parse_addition_expression("54 + 98"),
                Some((Add(Box::new(Constant(54)), Box::new(Constant(98))), ""))
            );
        }

        #[test]
        fn multiple_dice_and_constant() {
            assert_eq!(
                parse_addition_expression("4d4 + 98"),
                Some((
                    Add(Box::new(MultipleDice(4, D4)), Box::new(Constant(98))),
                    ""
                ))
            );
        }
    }

    mod full_expressions {
        use super::super::*;
        use crate::die::Die::*;
        use crate::expression::Expression::*;

        #[test]
        fn addition_then_multiplication() {
            let expected = Add(
                Box::new(MultipleDice(8, D6)),
                Box::new(Multiply(
                    Box::new(Constant(4)),
                    Box::new(MultipleDice(4, D4)),
                )),
            );
            assert_eq!(parse_expression("8d6 + 4 * 4d4"), Some((expected, "")));
        }

        #[test]
        fn multiplication_then_addition() {
            let expected = Add(
                Box::new(Multiply(
                    Box::new(Constant(4)),
                    Box::new(MultipleDice(4, D4)),
                )),
                Box::new(MultipleDice(8, D6)),
            );
            assert_eq!(parse_expression("4 * 4d4 + 8d6"), Some((expected, "")));
        }

        #[test]
        fn two_multiplications() {
            let expected = Multiply(
                Box::new(Constant(5)),
                Box::new(Multiply(
                    Box::new(SingleDie(D6)),
                    Box::new(MultipleDice(2, D20)),
                )),
            );
            assert_eq!(parse_expression("5 * d6 * 2d20"), Some((expected, "")));
        }

        #[test]
        fn two_additions() {
            let expected = Add(
                Box::new(Constant(5)),
                Box::new(Add(Box::new(SingleDie(D6)), Box::new(MultipleDice(2, D20)))),
            );
            assert_eq!(parse_expression("5 + d6 + 2d20"), Some((expected, "")));
        }

        #[test]
        fn brackets() {
            let expected = Multiply(
                Box::new(Add(Box::new(Constant(5)), Box::new(SingleDie(D4)))),
                Box::new(Constant(2)),
            );
            assert_eq!(parse_expression("(5 + d4) * 2"), Some((expected, "")));
        }
    }
}
