pub trait FunctionDeclaration
{
	fn new(name: String, arity: usize) -> Self;

	fn name(&self) -> &str;
	fn arity(&self) -> usize;
}

impl FunctionDeclaration for crate::FunctionDeclaration
{
	fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}

	fn name(&self) -> &str
	{
		&self.name
	}

	fn arity(&self) -> usize
	{
		self.arity
	}
}

pub trait PredicateDeclaration
{
	fn new(name: String, arity: usize) -> Self;

	fn name(&self) -> &str;
	fn arity(&self) -> usize;
}

impl PredicateDeclaration for crate::PredicateDeclaration
{
	fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}

	fn name(&self) -> &str
	{
		&self.name
	}

	fn arity(&self) -> usize
	{
		self.arity
	}
}

pub trait VariableDeclaration
{
	fn new(name: String) -> Self;

	fn name(&self) -> &str;
}

impl VariableDeclaration for crate::VariableDeclaration
{
	fn new(name: String) -> Self
	{
		Self
		{
			name
		}
	}

	fn name(&self) -> &str
	{
		&self.name
	}
}

pub trait Flavor
{
	type FunctionDeclaration: FunctionDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash;
	type PredicateDeclaration:
		PredicateDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash;
	type VariableDeclaration: VariableDeclaration + std::cmp::Eq + std::cmp::Ord + std::hash::Hash
		+ std::fmt::Display;
}

pub struct DefaultFlavor;

impl Flavor for DefaultFlavor
{
	type FunctionDeclaration = crate::FunctionDeclaration;
	type PredicateDeclaration = crate::PredicateDeclaration;
	type VariableDeclaration = crate::VariableDeclaration;
}
