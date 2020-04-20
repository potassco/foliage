use nom::
{
	IResult,
	branch::alt,
	bytes::complete::{escaped_transform, tag},
	character::complete::{digit1, none_of},
	combinator::{map, map_res, opt, recognize},
	sequence::{delimited, pair, terminated},
};

use super::word_boundary;

fn true_(i: &str) -> IResult<&str, bool>
{
	map
	(
		terminated
		(
			tag("true"),
			word_boundary,
		),
		|_| true,
	)(i)
}

fn false_(i: &str) -> IResult<&str, bool>
{
	map
	(
		terminated
		(
			tag("false"),
			word_boundary,
		),
		|_| false,
	)(i)
}

pub fn boolean(i: &str) -> IResult<&str, bool>
{
	alt
	((
		true_,
		false_,
	))(i)
}

pub fn integer(i: &str) -> IResult<&str, i32>
{
	map_res
	(
		recognize
		(
			terminated
			(
				pair
				(
					opt
					(
						alt
						((
							tag("-"),
							tag("+"),
						))
					),
					digit1,
				),
				word_boundary,
			)
		),
		std::str::FromStr::from_str,
	)(i)
}

fn infimum(i: &str) -> IResult<&str, crate::SpecialInteger>
{
	map
	(
		terminated
		(
			tag("#inf"),
			word_boundary,
		),
		|_| crate::SpecialInteger::Infimum,
	)(i)
}

fn supremum(i: &str) -> IResult<&str, crate::SpecialInteger>
{
	map
	(
		terminated
		(
			tag("#sup"),
			word_boundary,
		),
		|_| crate::SpecialInteger::Supremum,
	)(i)
}

pub fn special_integer(i: &str) -> IResult<&str, crate::SpecialInteger>
{
	alt
	((
		infimum,
		supremum,
	))(i)
}

pub fn string(i: &str) -> IResult<&str, String>
{
	map
	(
		terminated
		(
			delimited
			(
				tag("\""),
				escaped_transform
				(
					none_of("\"\\"),
					'\\',
					alt
					((
						tag("\""),
						tag("\\"),
						map
						(
							tag("n"),
							|_| "\n",
						),
						map
						(
							tag("t"),
							|_| "\t",
						),
					)),
				),
				tag("\""),
			),
			word_boundary,
		),
		String::from,
	)(i)
}

#[cfg(test)]
mod tests
{
	use crate::SpecialInteger;
	use crate::parse::*;

	#[test]
	fn parse_boolean()
	{
		assert_eq!(boolean("true"), Ok(("", true)));
		assert_eq!(boolean("false"), Ok(("", false)));
		assert_eq!(boolean("true false"), Ok((" false", true)));
		assert_eq!(boolean("false true"), Ok((" true", false)));
		assert_eq!(boolean("true,"), Ok((",", true)));
		assert_eq!(boolean("false,"), Ok((",", false)));
		assert!(boolean("truefalse").is_err());
		assert!(boolean("falsetrue").is_err());
		assert!(boolean("truea").is_err());
		assert!(boolean("falsea").is_err());
		assert!(boolean("a").is_err());
		assert!(boolean("-").is_err());
		assert!(boolean(" ").is_err());
	}

	#[test]
	fn parse_integer()
	{
		assert_eq!(integer("0"), Ok(("", 0)));
		assert_eq!(integer("10000"), Ok(("", 10000)));
		assert_eq!(integer("+10000"), Ok(("", 10000)));
		assert_eq!(integer("-10000"), Ok(("", -10000)));
		assert_eq!(integer("0 42"), Ok((" 42", 0)));
		assert_eq!(integer("10000 42"), Ok((" 42", 10000)));
		assert_eq!(integer("+10000 42"), Ok((" 42", 10000)));
		assert_eq!(integer("-10000 42"), Ok((" 42", -10000)));
		assert_eq!(integer("10000,"), Ok((",", 10000)));
		assert_eq!(integer("+10000,"), Ok((",", 10000)));
		assert_eq!(integer("-10000,"), Ok((",", -10000)));
		assert!(integer("10000a").is_err());
		assert!(integer("+10000a").is_err());
		assert!(integer("-10000a").is_err());
		assert_eq!(integer("1.5"), Ok((".5", 1)));
		assert!(integer("a").is_err());
		assert!(integer("-").is_err());
		assert!(integer(" ").is_err());
	}

	#[test]
	fn parse_special_integer()
	{
		assert_eq!(special_integer("#inf"), Ok(("", SpecialInteger::Infimum)));
		assert_eq!(special_integer("#sup"), Ok(("", SpecialInteger::Supremum)));
		assert_eq!(special_integer("#inf #sup"), Ok((" #sup", SpecialInteger::Infimum)));
		assert_eq!(special_integer("#sup #inf"), Ok((" #inf", SpecialInteger::Supremum)));
		assert_eq!(special_integer("#inf,"), Ok((",", SpecialInteger::Infimum)));
		assert_eq!(special_integer("#sup,"), Ok((",", SpecialInteger::Supremum)));
		assert!(special_integer("#inf0").is_err());
		assert!(special_integer("#sup0").is_err());
		assert!(special_integer("#infimum").is_err());
		assert!(special_integer("#supremum").is_err());
		assert!(special_integer("inf").is_err());
		assert!(special_integer("sup").is_err());
		assert!(special_integer("0").is_err());
		assert!(special_integer("10000").is_err());
		assert!(special_integer("-10000").is_err());
		assert!(special_integer("-").is_err());
		assert!(special_integer("+").is_err());
		assert!(special_integer("a").is_err());
		assert!(special_integer(" ").is_err());
	}

	#[test]
	fn parse_string()
	{
		assert_eq!(string("\"test 123\""), Ok(("", "test 123".to_string())));
		assert_eq!(string("\"123 test\""), Ok(("", "123 test".to_string())));
		assert_eq!(string("\" test 123 \""), Ok(("", " test 123 ".to_string())));
		assert_eq!(string("\"test 123\" \"rest"), Ok((" \"rest", "test 123".to_string())));
		assert_eq!(string("\"test 123\", \"rest"), Ok((", \"rest", "test 123".to_string())));
		assert_eq!(string("\"test\n123\""), Ok(("", "test\n123".to_string())));
		assert_eq!(string("\"test\\\"123\""), Ok(("", "test\"123".to_string())));
		assert_eq!(string("\"test\\\"123\\\"\""), Ok(("", "test\"123\"".to_string())));
		assert_eq!(string("\"\\\"test 123\\\"\""), Ok(("", "\"test 123\"".to_string())));
		assert_eq!(string("\"test\\\\123\""), Ok(("", "test\\123".to_string())));
		assert_eq!(string("\"test\\\\123\\\\\""), Ok(("", "test\\123\\".to_string())));
		assert_eq!(string("\"\\\\test 123\\\\\""), Ok(("", "\\test 123\\".to_string())));
		assert_eq!(string("\"test\\n123\""), Ok(("", "test\n123".to_string())));
		assert_eq!(string("\"test\\n123\\n\""), Ok(("", "test\n123\n".to_string())));
		assert_eq!(string("\"\\ntest 123\\n\""), Ok(("", "\ntest 123\n".to_string())));
		assert_eq!(string("\"test\\t123\""), Ok(("", "test\t123".to_string())));
		assert_eq!(string("\"test\\t123\\t\""), Ok(("", "test\t123\t".to_string())));
		assert_eq!(string("\"\\ttest 123\\t\""), Ok(("", "\ttest 123\t".to_string())));
		assert_eq!(string("\"test 🙂 123\""), Ok(("", "test 🙂 123".to_string())));
		assert_eq!(string("\"🙂test 123\""), Ok(("", "🙂test 123".to_string())));
		assert_eq!(string("\"test 123🙂\""), Ok(("", "test 123🙂".to_string())));
		assert!(string("\"test 123\"a").is_err());
		assert!(string("\"test\\i123\"").is_err());
		assert!(string("\"test").is_err());
		assert!(string("test").is_err());
		assert!(string("-").is_err());
		assert!(string(" ").is_err());
	}
}
