use nom::
{
	IResult,
	branch::alt,
	bytes::complete::take_while_m_n,
	combinator::{map, peek, rest_len, verify},
};

fn is_character_word_boundary(c: char) -> bool
{
	if c.is_whitespace()
	{
		return true;
	}

	match c
	{
		'('
		| ')'
		| '>'
		| '<'
		| '='
		| ','
		| '+'
		| '-'
		| '*'
		| '/'
		| '%'
		| '|'
		| '#'
			=> true,
		_ => false,
	}
}

pub(crate) fn word_boundary(i: &str) -> IResult<&str, ()>
{
	peek
	(
		alt
		((
			// Accept word boundary characters
			map
			(
				take_while_m_n(1, 1, is_character_word_boundary),
				|_| (),
			),
			// Accept end of file
			map
			(
				verify
				(
					rest_len,
					|rest_length| *rest_length == 0usize,
				),
				|_| (),
			),
		))
	)(i)
}

#[cfg(test)]
mod tests
{
	use crate::parse::*;

	#[test]
	fn detect_word_boundaries()
	{
		assert_eq!(word_boundary(" rest"), Ok((" rest", ())));
		assert_eq!(word_boundary("(rest"), Ok(("(rest", ())));
		assert_eq!(word_boundary(")rest"), Ok((")rest", ())));
		assert_eq!(word_boundary(",rest"), Ok((",rest", ())));
		assert_eq!(word_boundary("+rest"), Ok(("+rest", ())));
		assert_eq!(word_boundary("-rest"), Ok(("-rest", ())));
		assert_eq!(word_boundary("*rest"), Ok(("*rest", ())));
		assert_eq!(word_boundary("/rest"), Ok(("/rest", ())));
		assert_eq!(word_boundary("%rest"), Ok(("%rest", ())));
		assert_eq!(word_boundary("|rest"), Ok(("|rest", ())));
		assert_eq!(word_boundary("<rest"), Ok(("<rest", ())));
		assert_eq!(word_boundary(">rest"), Ok((">rest", ())));
		assert_eq!(word_boundary("=rest"), Ok(("=rest", ())));
		assert!(word_boundary("0").is_err());
		assert!(word_boundary("rest").is_err());
	}
}
