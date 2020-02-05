# foliage [![GitHub release](https://img.shields.io/github/release/potassco/foliage.svg?maxAge=3600)](https://github.com/potassco/foliage/releases) [![crates.io](https://img.shields.io/crates/v/foliage.svg?maxAge=3600)](https://crates.io/crates/foliage)

> First-order logic with integer arithmetics in Rust

This Rust crate provides an abstract syntax tree for first-order formulas with integer arithmetics.

## Supported Formulas

- Booleans values (`true` and `false`)
- predicates
- negated formulas
- comparisons of terms (<, ≤, >, ≥, =, ≠)
- implications and biconditionals
- conjunctions and disjunctions of formulas
- existentially and universally quantified formulas

## Supported Terms

- Boolean values (`true` and `false`)
- integers
- strings
- special integers (infimum and supremum)
- symbolic functions
- variables
- binary operations (addition, subtraction, multiplication, division, modulo, exponentiation)
- unary operations (absolute value, numeric negation)
