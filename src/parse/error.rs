pub type Source = Box<dyn std::error::Error>;

pub struct Location
{
	start: usize,
	end: Option<usize>,
}

impl Location
{
	pub fn new(start: usize, end: Option<usize>) -> Self
	{
		Self
		{
			start,
			end,
		}
	}
}

pub enum Kind
{
	UnmatchedParenthesis,
	CharacterNotAllowed(char),
	ParseNumber(String),
	MixedImplicationDirections(Location),
	ExpectedVariableDeclaration,
	UnexpectedToken,
	EmptyExpression,
	ExpectedLogicalConnectiveArgument(String),
	MultipleComparisonOperators(crate::ComparisonOperator, crate::ComparisonOperator),
}

pub struct Error
{
	pub kind: Kind,
	pub location: Location,
	pub source: Option<Source>,
}

impl Error
{
	pub(crate) fn new(kind: Kind, location: Location) -> Self
	{
		Self
		{
			kind,
			location,
			source: None,
		}
	}

	pub(crate) fn with<S: Into<Source>>(mut self, source: S) -> Self
	{
		self.source = Some(source.into());
		self
	}

	pub(crate) fn new_unmatched_parenthesis(location: Location) -> Self
	{
		Self::new(Kind::UnmatchedParenthesis, location)
	}

	pub(crate) fn new_character_not_allowed(character: char, location: Location) -> Self
	{
		Self::new(Kind::CharacterNotAllowed(character), location)
	}

	pub(crate) fn new_parse_number<I: Into<String>, S: Into<Source>>(input: I, location: Location,
		source: S)
		-> Self
	{
		Self::new(Kind::ParseNumber(input.into()), location).with(source)
	}

	pub(crate) fn new_mixed_implication_directions(location_1: Location, location_2: Location)
		-> Self
	{
		Self::new(Kind::MixedImplicationDirections(location_2), location_1)
	}

	pub(crate) fn new_expected_variable_declaration(location: Location) -> Self
	{
		Self::new(Kind::ExpectedVariableDeclaration, location)
	}

	pub(crate) fn new_unexpected_token(location: Location) -> Self
	{
		Self::new(Kind::UnexpectedToken, location)
	}

	pub(crate) fn new_empty_expression(location: Location) -> Self
	{
		Self::new(Kind::EmptyExpression, location)
	}

	pub(crate) fn new_expected_logical_connective_argument(logical_connective_name: String,
		location: Location)
		-> Self
	{
		Self::new(Kind::ExpectedLogicalConnectiveArgument(logical_connective_name), location)
	}

	pub(crate) fn new_multiple_comparison_operators(
		comparison_operator_1: crate::ComparisonOperator,
		comparison_operator_2: crate::ComparisonOperator, location: Location)
		-> Self
	{
		Self::new(Kind::MultipleComparisonOperators(comparison_operator_1, comparison_operator_2),
			location)
	}
}

impl std::fmt::Debug for Error
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		if let Some(end) = self.location.end
		{
			write!(formatter, "parsing error at {}:{}: ", self.location.start, end)?;
		}
		else
		{
			write!(formatter, "parsing error at {}: ", self.location.start)?;
		}

		match &self.kind
		{
			Kind::UnmatchedParenthesis => write!(formatter, "unmatched parenthesis")?,
			Kind::CharacterNotAllowed(character) =>
				write!(formatter, "character not allowed: ‘{}’", character)?,
			Kind::ParseNumber(input) => write!(formatter, "could not “{}” as number", input)?,
			// TODO: print second location properly
			Kind::MixedImplicationDirections(_location_2) =>
				write!(formatter, "-> and <- implications may not be mixed within the same scope")?,
			Kind::ExpectedVariableDeclaration =>
				write!(formatter, "expected variable declaration")?,
			Kind::UnexpectedToken => write!(formatter, "unexpected token")?,
			Kind::EmptyExpression => write!(formatter, "empty expression")?,
			Kind::ExpectedLogicalConnectiveArgument(ref logical_connective_name) =>
				write!(formatter, "this “{}” logical connective is missing an argument",
					logical_connective_name)?,
			Kind::MultipleComparisonOperators(comparison_operator_1, comparison_operator_2) =>
				write!(formatter, "chained comparisons aren’t supported (found “{:?}” and “{:?}” in the same formula), consider separating them with “and”",
					comparison_operator_1, comparison_operator_2)?,
		}

		if let Some(source) = &self.source
		{
			write!(formatter, "\nerror source: {}", source)?;
		}

		Ok(())
	}
}

impl std::fmt::Display for Error
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{:?}", self)
	}
}

impl std::error::Error for Error
{
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)>
	{
		match &self.source
		{
			Some(source) => Some(source.as_ref()),
			None => None,
		}
	}
}
