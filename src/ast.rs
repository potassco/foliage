use crate::flavor::{FunctionDeclaration as _, PredicateDeclaration as _};

// Operators

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum BinaryOperator
{
	Add,
	Subtract,
	Multiply,
	Divide,
	Modulo,
	Exponentiate,
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ComparisonOperator
{
	Greater,
	Less,
	LessOrEqual,
	GreaterOrEqual,
	NotEqual,
	Equal,
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum UnaryOperator
{
	AbsoluteValue,
	Negative,
}

// ImplicationDirection

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ImplicationDirection
{
	LeftToRight,
	RightToLeft,
}

// Primitives

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FunctionDeclaration
{
	pub name: String,
	pub arity: usize,
}

impl FunctionDeclaration
{
	pub fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}
}

pub type FunctionDeclarations<F> =
	std::collections::BTreeSet<std::rc::Rc<<F as crate::flavor::Flavor>::FunctionDeclaration>>;

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PredicateDeclaration
{
	pub name: String,
	pub arity: usize,
}

impl PredicateDeclaration
{
	pub fn new(name: String, arity: usize) -> Self
	{
		Self
		{
			name,
			arity,
		}
	}
}

pub type PredicateDeclarations<F> =
	std::collections::BTreeSet<std::rc::Rc<<F as crate::flavor::Flavor>::PredicateDeclaration>>;

pub struct VariableDeclaration
{
	pub name: String,
}

impl std::cmp::PartialEq for VariableDeclaration
{
	#[inline(always)]
	fn eq(&self, other: &Self) -> bool
	{
		let l = self as *const Self;
		let r = other as *const Self;

		l.eq(&r)
	}
}

impl std::cmp::Eq for VariableDeclaration
{
}

impl std::cmp::PartialOrd for VariableDeclaration
{
	#[inline(always)]
	fn partial_cmp(&self, other: &VariableDeclaration) -> Option<std::cmp::Ordering>
	{
		let l = self as *const VariableDeclaration;
		let r = other as *const VariableDeclaration;

		l.partial_cmp(&r)
	}
}

impl std::cmp::Ord for VariableDeclaration
{
	#[inline(always)]
	fn cmp(&self, other: &VariableDeclaration) -> std::cmp::Ordering
	{
		let l = self as *const VariableDeclaration;
		let r = other as *const VariableDeclaration;

		l.cmp(&r)
	}
}

impl std::hash::Hash for VariableDeclaration
{
	#[inline(always)]
	fn hash<H: std::hash::Hasher>(&self, state: &mut H)
	{
		let p = self as *const VariableDeclaration;

		p.hash(state);
	}
}

impl VariableDeclaration
{
	pub fn new(name: String) -> Self
	{
		Self
		{
			name,
		}
	}
}

pub type VariableDeclarations<F> =
	Vec<std::rc::Rc<<F as crate::flavor::Flavor>::VariableDeclaration>>;

// Terms

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct BinaryOperation<F>
where
	F: crate::flavor::Flavor,
{
	pub operator: BinaryOperator,
	pub left: Box<Term<F>>,
	pub right: Box<Term<F>>,
}

impl<F> BinaryOperation<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(operator: BinaryOperator, left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self
		{
			operator,
			left,
			right,
		}
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Function<F>
where
	F: crate::flavor::Flavor,
{
	pub declaration: std::rc::Rc<F::FunctionDeclaration>,
	pub arguments: Terms<F>,
}

impl<F> Function<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(declaration: std::rc::Rc<F::FunctionDeclaration>, arguments: Terms<F>) -> Self
	{
		assert_eq!(declaration.arity(), arguments.len(),
			"function has a different number of arguments then declared");

		Self
		{
			declaration,
			arguments,
		}
	}
}

#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SpecialInteger
{
	Infimum,
	Supremum,
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct UnaryOperation<F>
where
	F: crate::flavor::Flavor,
{
	pub operator: UnaryOperator,
	pub argument: Box<Term<F>>,
}

impl<F> UnaryOperation<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(operator: UnaryOperator, argument: Box<Term<F>>) -> Self
	{
		Self
		{
			operator,
			argument,
		}
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Variable<F>
where
	F: crate::flavor::Flavor,
{
	pub declaration: std::rc::Rc<F::VariableDeclaration>,
}

impl<F> Variable<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(declaration: std::rc::Rc<F::VariableDeclaration>) -> Self
	{
		Self
		{
			declaration,
		}
	}
}

// Formulas

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Compare<F>
where
	F: crate::flavor::Flavor,
{
	pub operator: ComparisonOperator,
	pub left: Box<Term<F>>,
	pub right: Box<Term<F>>,
}

impl<F> Compare<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(operator: ComparisonOperator, left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self
		{
			operator,
			left,
			right,
		}
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct QuantifiedFormula<F>
where
	F: crate::flavor::Flavor,
{
	pub parameters: std::rc::Rc<VariableDeclarations<F>>,
	pub argument: Box<Formula<F>>,
}

impl<F> QuantifiedFormula<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(parameters: std::rc::Rc<VariableDeclarations<F>>, argument: Box<Formula<F>>) -> Self
	{
		Self
		{
			parameters,
			argument,
		}
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Implies<F>
where
	F: crate::flavor::Flavor,
{
	pub direction: ImplicationDirection,
	pub antecedent: Box<Formula<F>>,
	pub implication: Box<Formula<F>>,
}

impl<F> Implies<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(direction: ImplicationDirection, antecedent: Box<Formula<F>>,
		implication: Box<Formula<F>>)
		-> Self
	{
		Self
		{
			direction,
			antecedent,
			implication,
		}
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Predicate<F>
where
	F: crate::flavor::Flavor,
{
	pub declaration: std::rc::Rc<F::PredicateDeclaration>,
	pub arguments: Terms<F>,
}

impl<F> Predicate<F>
where
	F: crate::flavor::Flavor,
{
	pub fn new(declaration: std::rc::Rc<F::PredicateDeclaration>, arguments: Terms<F>) -> Self
	{
		assert_eq!(declaration.arity(), arguments.len(),
			"predicate has a different number of arguments then declared");

		Self
		{
			declaration,
			arguments,
		}
	}
}

// Variants

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Term<F>
where
	F: crate::flavor::Flavor,
{
	BinaryOperation(BinaryOperation<F>),
	Boolean(bool),
	Function(Function<F>),
	Integer(i32),
	SpecialInteger(SpecialInteger),
	String(String),
	UnaryOperation(UnaryOperation<F>),
	Variable(Variable<F>),
}

pub type Terms<F> = Vec<Term<F>>;

impl<F> Term<F>
where
	F: crate::flavor::Flavor,
{
	pub fn absolute_value(argument: Box<Term<F>>) -> Self
	{
		Self::unary_operation(UnaryOperator::AbsoluteValue, argument)
	}

	pub fn add(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Add, left, right)
	}

	pub fn binary_operation(operator: BinaryOperator, left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::BinaryOperation(BinaryOperation::new(operator, left, right))
	}

	pub fn boolean(value: bool) -> Self
	{
		Self::Boolean(value)
	}

	pub fn divide(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Divide, left, right)
	}

	pub fn exponentiate(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Exponentiate, left, right)
	}

	pub fn false_() -> Self
	{
		Self::boolean(false)
	}

	pub fn function(declaration: std::rc::Rc<F::FunctionDeclaration>, arguments: Terms<F>) -> Self
	{
		Self::Function(Function::new(declaration, arguments))
	}

	pub fn infimum() -> Self
	{
		Self::special_integer(SpecialInteger::Infimum)
	}

	pub fn integer(value: i32) -> Self
	{
		Self::Integer(value)
	}

	pub fn modulo(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Modulo, left, right)
	}

	pub fn multiply(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Multiply, left, right)
	}

	pub fn negative(argument: Box<Term<F>>) -> Self
	{
		Self::unary_operation(UnaryOperator::Negative, argument)
	}

	pub fn special_integer(value: SpecialInteger) -> Self
	{
		Self::SpecialInteger(value)
	}

	pub fn string(value: String) -> Self
	{
		Self::String(value)
	}

	pub fn subtract(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::binary_operation(BinaryOperator::Subtract, left, right)
	}

	pub fn supremum() -> Self
	{
		Self::special_integer(SpecialInteger::Supremum)
	}

	pub fn true_() -> Self
	{
		Self::boolean(true)
	}

	pub fn unary_operation(operator: UnaryOperator, argument: Box<Term<F>>) -> Self
	{
		Self::UnaryOperation(UnaryOperation::new(operator, argument))
	}

	pub fn variable(declaration: std::rc::Rc<F::VariableDeclaration>) -> Self
	{
		Self::Variable(Variable::new(declaration))
	}
}

#[derive(Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Formula<F>
where
	F: crate::flavor::Flavor,
{
	And(Formulas<F>),
	Boolean(bool),
	Compare(Compare<F>),
	Exists(QuantifiedFormula<F>),
	ForAll(QuantifiedFormula<F>),
	IfAndOnlyIf(Formulas<F>),
	Implies(Implies<F>),
	Not(Box<Formula<F>>),
	Or(Formulas<F>),
	Predicate(Predicate<F>),
}

pub type Formulas<F> = Vec<Formula<F>>;

impl<F> Formula<F>
where
	F: crate::flavor::Flavor,
{
	pub fn and(arguments: Formulas<F>) -> Self
	{
		Self::And(arguments)
	}

	pub fn boolean(value: bool) -> Self
	{
		Self::Boolean(value)
	}

	pub fn compare(operator: ComparisonOperator, left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::Compare(Compare::new(operator, left, right))
	}

	pub fn exists(parameters: std::rc::Rc<VariableDeclarations<F>>, argument: Box<Formula<F>>) -> Self
	{
		Self::Exists(QuantifiedFormula::new(parameters, argument))
	}

	pub fn equal(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::Equal, left, right)
	}

	pub fn false_() -> Self
	{
		Self::boolean(false)
	}

	pub fn for_all(parameters: std::rc::Rc<VariableDeclarations<F>>, argument: Box<Formula<F>>) -> Self
	{
		Self::ForAll(QuantifiedFormula::new(parameters, argument))
	}

	pub fn greater(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::Greater, left, right)
	}

	pub fn greater_or_equal(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::GreaterOrEqual, left, right)
	}

	pub fn if_and_only_if(arguments: Formulas<F>) -> Self
	{
		Self::IfAndOnlyIf(arguments)
	}

	pub fn implies(direction: ImplicationDirection, antecedent: Box<Formula<F>>,
		consequent: Box<Formula<F>>) -> Self
	{
		Self::Implies(Implies::new(direction, antecedent, consequent))
	}

	pub fn less(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::Less, left, right)
	}

	pub fn less_or_equal(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::LessOrEqual, left, right)
	}

	pub fn not(argument: Box<Formula<F>>) -> Self
	{
		Self::Not(argument)
	}

	pub fn not_equal(left: Box<Term<F>>, right: Box<Term<F>>) -> Self
	{
		Self::compare(ComparisonOperator::NotEqual, left, right)
	}

	pub fn or(arguments: Formulas<F>) -> Self
	{
		Self::Or(arguments)
	}

	pub fn predicate(declaration: std::rc::Rc<F::PredicateDeclaration>, arguments: Terms<F>) -> Self
	{
		Self::Predicate(Predicate::new(declaration, arguments))
	}

	pub fn true_() -> Self
	{
		Self::boolean(true)
	}
}

pub struct OpenFormula<F>
where
	F: crate::flavor::Flavor,
{
	pub free_variable_declarations: std::rc::Rc<VariableDeclarations<F>>,
	pub formula: Formula<F>,
}
