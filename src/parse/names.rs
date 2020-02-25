use nom::
{
	IResult,
	bytes::complete::{take_while, take_while_m_n},
	combinator::recognize,
	sequence::{pair, terminated},
};

use super::word_boundary;

fn is_function_name_character_first(c: char) -> bool
{
	c.is_alphabetic() && c.is_lowercase()
}

fn is_function_name_character_body(c: char) -> bool
{
	c.is_alphanumeric() || c == '_'
}

fn is_variable_name_character_first(c: char) -> bool
{
	c.is_alphabetic() && c.is_uppercase()
}

fn is_variable_name_character_body(c: char) -> bool
{
	c.is_alphanumeric() || c == '_'
}

pub fn function_or_predicate_name(i: &str) -> IResult<&str, &str>
{
	let (i, name) =
		recognize
		(
			terminated
			(
				pair
				(
					take_while_m_n(1, 1, is_function_name_character_first),
					take_while(is_function_name_character_body),
				),
				word_boundary,
			)
		)(i)?;

	match name
	{
		"and"
		| "exists"
		| "false"
		| "forall"
		| "not"
		| "or"
		| "true"
			=> Err(nom::Err::Error((i, nom::error::ErrorKind::Verify))),
		name => Ok((i, name)),
	}
}

pub fn variable_name(i: &str) -> IResult<&str, &str>
{
	recognize
	(
		terminated
		(
			pair
			(
				take_while_m_n(1, 1, is_variable_name_character_first),
				take_while(is_variable_name_character_body),
			),
			word_boundary,
		)
	)(i)
}

#[cfg(test)]
mod tests
{
	use crate::parse::*;

	#[test]
	fn parse_function_or_predicate_name()
	{
		assert_eq!(function_or_predicate_name("p rest"), Ok((" rest", "p")));
		assert_eq!(function_or_predicate_name("f rest"), Ok((" rest", "f")));
		assert_eq!(function_or_predicate_name("p, rest"), Ok((", rest", "p")));
		assert_eq!(function_or_predicate_name("f, rest"), Ok((", rest", "f")));
		assert_eq!(function_or_predicate_name("name_123 rest"), Ok((" rest", "name_123")));
		assert!(function_or_predicate_name("0 rest").is_err());
		assert!(function_or_predicate_name("123_asd rest").is_err());
		assert!(function_or_predicate_name("P rest").is_err());
		assert!(function_or_predicate_name("Predicate_123 rest").is_err());
		assert!(function_or_predicate_name("_ rest").is_err());
		assert!(function_or_predicate_name("_predicate_123 rest").is_err());
		assert!(function_or_predicate_name("(p").is_err());
		assert!(function_or_predicate_name(")p").is_err());
		assert!(function_or_predicate_name(">p").is_err());
		assert!(function_or_predicate_name("<p").is_err());
		assert!(function_or_predicate_name("=p").is_err());
		assert!(function_or_predicate_name(",p").is_err());
		assert!(function_or_predicate_name("+p").is_err());
		assert!(function_or_predicate_name("-p").is_err());
		assert!(function_or_predicate_name("*p").is_err());
		assert!(function_or_predicate_name("/p").is_err());
		assert!(function_or_predicate_name("%p").is_err());
		assert!(function_or_predicate_name("|p").is_err());
		assert!(function_or_predicate_name("#inf").is_err());
		assert!(function_or_predicate_name("#sup").is_err());
		assert!(function_or_predicate_name("#p").is_err());
		assert!(function_or_predicate_name(" ").is_err());
		// Keywords aren’t valid names
		assert!(function_or_predicate_name("and rest").is_err());
		assert!(function_or_predicate_name("exists rest").is_err());
		assert!(function_or_predicate_name("false rest").is_err());
		assert!(function_or_predicate_name("forall rest").is_err());
		assert!(function_or_predicate_name("or rest").is_err());
		assert!(function_or_predicate_name("not rest").is_err());
		assert!(function_or_predicate_name("true rest").is_err());
		// Names that start with keywords are fine though
		assert!(function_or_predicate_name("anda rest").is_ok());
		assert!(function_or_predicate_name("existsa rest").is_ok());
		assert!(function_or_predicate_name("falsea rest").is_ok());
		assert!(function_or_predicate_name("foralla rest").is_ok());
		assert!(function_or_predicate_name("ora rest").is_ok());
		assert!(function_or_predicate_name("nota rest").is_ok());
		assert!(function_or_predicate_name("truea rest").is_ok());
	}

	#[test]
	fn parse_variable_name()
	{
		assert_eq!(variable_name("X Rest"), Ok((" Rest", "X")));
		assert_eq!(variable_name("X, Rest"), Ok((", Rest", "X")));
		assert_eq!(variable_name("Variable_123 Rest"), Ok((" Rest", "Variable_123")));
		assert!(variable_name("0 Rest").is_err());
		assert!(variable_name("123_Asd Rest").is_err());
		assert!(variable_name("x Rest").is_err());
		assert!(variable_name("variable_123 Rest").is_err());
		assert!(variable_name("_ Rest").is_err());
		assert!(variable_name("_variable_123 Rest").is_err());
		assert!(variable_name("(X").is_err());
		assert!(variable_name(")X").is_err());
		assert!(variable_name(">X").is_err());
		assert!(variable_name("<X").is_err());
		assert!(variable_name("=X").is_err());
		assert!(variable_name(",X").is_err());
		assert!(variable_name("+X").is_err());
		assert!(variable_name("-X").is_err());
		assert!(variable_name("*X").is_err());
		assert!(variable_name("/X").is_err());
		assert!(variable_name("%X").is_err());
		assert!(variable_name("|X").is_err());
		assert!(variable_name("#inf").is_err());
		assert!(variable_name("#sup").is_err());
		assert!(variable_name("#X").is_err());
		assert!(variable_name(" ").is_err());
	}
}
