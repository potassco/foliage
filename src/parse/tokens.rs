#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum Symbol
{
	ArrowLeft,
	ArrowLeftAndRight,
	ArrowRight,
	Comma,
	Division,
	Equal,
	Exponentiation,
	Greater,
	GreaterOrEqual,
	Less,
	LessOrEqual,
	Minus,
	Multiplication,
	NotEqual,
	Plus,
	VerticalBar,
}

impl std::fmt::Debug for Symbol
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self
		{
			Self::ArrowLeft => write!(formatter, "<-"),
			Self::ArrowLeftAndRight => write!(formatter, "<->"),
			Self::ArrowRight => write!(formatter, "->"),
			Self::Comma => write!(formatter, ","),
			Self::Division => write!(formatter, "/"),
			Self::Equal => write!(formatter, "="),
			Self::Exponentiation => write!(formatter, "**"),
			Self::Greater => write!(formatter, ">"),
			Self::GreaterOrEqual => write!(formatter, ">="),
			Self::Less => write!(formatter, "<"),
			Self::LessOrEqual => write!(formatter, "<="),
			Self::Minus => write!(formatter, "-"),
			Self::Multiplication => write!(formatter, "*"),
			Self::NotEqual => write!(formatter, "!="),
			Self::Plus => write!(formatter, "+"),
			Self::VerticalBar => write!(formatter, "|"),
		}
	}
}

fn is_identifier_start_character(character: char) -> bool
{
	// TODO: support leading underscores
	character.is_ascii_alphabetic()
}

fn is_identifier_body_character(character: char) -> bool
{
	match character
	{
		'_' => true,
		_ if character.is_ascii_alphanumeric() => true,
		_ => false,
	}
}

pub(crate) fn identifier(input: &str) -> Option<(&str, &str)>
{
	let mut characters = input.char_indices();

	let (_, character) = match characters.next()
	{
		Some(characters_next) => characters_next,
		None => return None,
	};

	if !is_identifier_start_character(character)
	{
		return None;
	}

	loop
	{
		match characters.next()
		{
			None => return Some((input, characters.as_str())),
			Some((character_index, character)) =>
			{
				if !is_identifier_body_character(character)
				{
					return Some(input.split_at(character_index));
				}
			},
		}
	}
}

fn number_string(input: &str) -> Option<(&str, &str)>
{
	let mut characters = input.char_indices();

	let (_, character) = match characters.next()
	{
		Some(characters_next) => characters_next,
		None => return None,
	};

	if !character.is_ascii_digit()
	{
		return None;
	}

	loop
	{
		match characters.next()
		{
			None => return Some((input, characters.as_str())),
			Some((character_index, character)) =>
			{
				if !character.is_ascii_digit()
				{
					return Some(input.split_at(character_index));
				}
			},
		}
	}
}

pub(crate) fn number(input: &str) -> Result<Option<(usize, &str)>, crate::parse::Error>
{
	let (number_string, remaining_input) = match number_string(input)
	{
		Some(number_string) => number_string,
		None => return Ok(None),
	};

	let number = number_string.parse()
		.map_err(|error| crate::parse::Error::new_parse_number(input,
			crate::parse::error::Location::new(0, Some(0)), error))?;

	Ok(Some((number, remaining_input)))
}

pub(crate) fn symbol(input: &str) -> Option<(Symbol, &str)>
{
	let mut characters = input.char_indices();

	let (_, character) = match characters.next()
	{
		Some(characters_next) => characters_next,
		None => return None,
	};

	let remaining_input = characters.as_str();

	match character
	{
		',' => Some((Symbol::Comma, remaining_input)),
		// <->, <-, <=, <
		'=' => Some((Symbol::Equal, remaining_input)),
		// !=
		'!' => match characters.next()
		{
			Some((_, '=')) => Some((Symbol::NotEqual, characters.as_str())),
			_ => None,
		},
		'<' => match characters.next()
		{
			Some((_, '-')) =>
			{
				let remaining_input = characters.as_str();

				match characters.next()
				{
					Some((_, '>')) => Some((Symbol::ArrowLeftAndRight, characters.as_str())),
					_ => Some((Symbol::ArrowLeft, remaining_input)),
				}
			},
			Some((_, '=')) => Some((Symbol::LessOrEqual, characters.as_str())),
			_ => Some((Symbol::Less, remaining_input)),
		},
		// >=, >
		'>' => match characters.next()
		{
			Some((_, '=')) => Some((Symbol::GreaterOrEqual, characters.as_str())),
			_ => Some((Symbol::Greater, remaining_input)),
		},
		'+' => Some((Symbol::Plus, remaining_input)),
		// ->, -
		'-' => match characters.next()
		{
			Some((_, '>')) => Some((Symbol::ArrowRight, characters.as_str())),
			_ => Some((Symbol::Minus, remaining_input)),
		},
		// **, *
		'*' => match characters.next()
		{
			Some((_, '*')) => Some((Symbol::Exponentiation, characters.as_str())),
			_ => Some((Symbol::Multiplication, remaining_input)),
		},
		'/' => Some((Symbol::Division, remaining_input)),
		'|' => Some((Symbol::VerticalBar, remaining_input)),
		_ => None,
	}
}

pub(crate) fn parenthesized_expression(input: &str)
	-> Result<Option<(&str, &str)>, crate::parse::Error>
{
	let mut characters = input.chars();

	let (first_character, remaining_input) = match characters.next()
	{
		Some(first_character) => (first_character, characters.as_str()),
		None => return Ok(None),
	};

	if first_character != '('
	{
		return Ok(None);
	}

	let mut characters = remaining_input.char_indices();
	let mut number_of_open_parentheses = 1;

	while let Some((character_index, character)) = characters.next()
	{
		match character
		{
			'(' => number_of_open_parentheses += 1,
			')' => number_of_open_parentheses -= 1,
			_ => (),
		}

		if number_of_open_parentheses == 0
		{
			let position_of_closing_parenthesis = character_index;
			let (parenthesized_expression, _) =
				remaining_input.split_at(position_of_closing_parenthesis);
			let remaining_input = characters.as_str();

			return Ok(Some((parenthesized_expression, remaining_input)));
		}
	}

	Err(crate::parse::Error::new_unmatched_parenthesis(
		crate::parse::error::Location::new(0, Some(1))))
}

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn tokenize_primitives()
	{
		assert_eq!(parenthesized_expression("(foo bar baz) test").unwrap(),
			Some(("foo bar baz", " test")));
		assert!(parenthesized_expression("( | asd#0231(asd|asd) test").is_err());
		assert_eq!(parenthesized_expression("( | asd#0231(asd|asd) ) test").unwrap(),
			Some((" | asd#0231(asd|asd) ", " test")));
		assert_eq!(parenthesized_expression("( | a)sd#0231(asd|asd)  test").unwrap(),
			Some((" | a", "sd#0231(asd|asd)  test")));

		assert_eq!(number("1234, ").unwrap(), Some((1234, ", ")));
		assert_eq!(number("1234.5, ").unwrap(), Some((1234, ".5, ")));
		assert_eq!(number("-1234, ").unwrap(), None);
		assert_eq!(number("a1234, ").unwrap(), None);

		assert_eq!(symbol("<-"), Some((Symbol::ArrowLeft, "")));
		assert_eq!(symbol("<->"), Some((Symbol::ArrowLeftAndRight, "")));
		assert_eq!(symbol("->"), Some((Symbol::ArrowRight, "")));
		assert_eq!(symbol(","), Some((Symbol::Comma, "")));
		assert_eq!(symbol("/"), Some((Symbol::Division, "")));
		assert_eq!(symbol("="), Some((Symbol::Equal, "")));
		assert_eq!(symbol("**"), Some((Symbol::Exponentiation, "")));
		assert_eq!(symbol(">"), Some((Symbol::Greater, "")));
		assert_eq!(symbol(">="), Some((Symbol::GreaterOrEqual, "")));
		assert_eq!(symbol("<"), Some((Symbol::Less, "")));
		assert_eq!(symbol("<="), Some((Symbol::LessOrEqual, "")));
		assert_eq!(symbol("-"), Some((Symbol::Minus, "")));
		assert_eq!(symbol("*"), Some((Symbol::Multiplication, "")));
		assert_eq!(symbol("!="), Some((Symbol::NotEqual, "")));
		assert_eq!(symbol("+"), Some((Symbol::Plus, "")));
		assert_eq!(symbol("|"), Some((Symbol::VerticalBar, "")));

		assert_eq!(symbol("<-a"), Some((Symbol::ArrowLeft, "a")));
		assert_eq!(symbol("<->a"), Some((Symbol::ArrowLeftAndRight, "a")));
		assert_eq!(symbol("->a"), Some((Symbol::ArrowRight, "a")));
		assert_eq!(symbol(",a"), Some((Symbol::Comma, "a")));
		assert_eq!(symbol("/a"), Some((Symbol::Division, "a")));
		assert_eq!(symbol("=a"), Some((Symbol::Equal, "a")));
		assert_eq!(symbol("**a"), Some((Symbol::Exponentiation, "a")));
		assert_eq!(symbol(">a"), Some((Symbol::Greater, "a")));
		assert_eq!(symbol(">=a"), Some((Symbol::GreaterOrEqual, "a")));
		assert_eq!(symbol("<a"), Some((Symbol::Less, "a")));
		assert_eq!(symbol("<=a"), Some((Symbol::LessOrEqual, "a")));
		assert_eq!(symbol("-a"), Some((Symbol::Minus, "a")));
		assert_eq!(symbol("*a"), Some((Symbol::Multiplication, "a")));
		assert_eq!(symbol("!=a"), Some((Symbol::NotEqual, "a")));
		assert_eq!(symbol("+a"), Some((Symbol::Plus, "a")));
		assert_eq!(symbol("|a"), Some((Symbol::VerticalBar, "a")));
	}
}
