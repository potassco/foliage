pub trait FunctionDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result;
	fn arity(&self) -> usize;
	fn matches_signature(&self, other_name: &str, other_arity: usize) -> bool;
}

impl FunctionDeclaration for crate::FunctionDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}", self.name)
	}

	fn arity(&self) -> usize
	{
		self.arity
	}

	fn matches_signature(&self, other_name: &str, other_arity: usize) -> bool
	{
		self.name == other_name && self.arity == other_arity
	}
}

pub trait PredicateDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result;
	fn arity(&self) -> usize;
	fn matches_signature(&self, other_name: &str, other_arity: usize) -> bool;
}

impl PredicateDeclaration for crate::PredicateDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}", self.name)
	}

	fn arity(&self) -> usize
	{
		self.arity
	}

	fn matches_signature(&self, other_name: &str, other_arity: usize) -> bool
	{
		self.name == other_name && self.arity == other_arity
	}
}

pub trait VariableDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result;
	fn matches_name(&self, other_name: &str) -> bool;
}

impl VariableDeclaration for crate::VariableDeclaration
{
	fn display_name(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(formatter, "{}", self.name)
	}

	fn matches_name(&self, other_name: &str) -> bool
	{
		self.name == other_name
	}
}

pub trait Flavor
{
	type FunctionDeclaration: FunctionDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash;
	type PredicateDeclaration:
		PredicateDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash;
	type VariableDeclaration: VariableDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash;
}

pub struct DefaultFlavor;

impl Flavor for DefaultFlavor
{
	type FunctionDeclaration = crate::FunctionDeclaration;
	type PredicateDeclaration = crate::PredicateDeclaration;
	type VariableDeclaration = crate::VariableDeclaration;
}
