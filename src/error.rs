pub type Source = Box<dyn std::error::Error>;

pub enum Kind
{
	Logic(&'static str),
}

pub struct Error
{
	pub kind: Kind,
	pub source: Option<Source>,
}

impl Error
{
	pub(crate) fn new(kind: Kind) -> Self
	{
		Self
		{
			kind,
			source: None,
		}
	}

	pub(crate) fn with<S: Into<Source>>(mut self, source: S) -> Self
	{
		self.source = Some(source.into());
		self
	}

	pub(crate) fn new_logic(description: &'static str) -> Self
	{
		Self::new(Kind::Logic(description))
	}
}

impl std::fmt::Debug for Error
{
	fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		match &self.kind
		{
			Kind::Logic(ref description) => write!(formatter,
				"logic error, please report to bug tracker ({})", description),
		}?;

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
