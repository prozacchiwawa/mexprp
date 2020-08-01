use std::fmt::Debug;
use std::rc::Rc;

use term::Term;
use context::Context;
use errors::MathError;
use num::Num;
use answer::Answer;

/// The result of an evaluation
pub type Calculation<N> = Result<Answer<N>, MathError>;

/// A trait for operations
pub trait Operate<N: Num>: Debug {
	/// Evalute the operation or return an error
	fn eval(&self, ctx: &Context<N>) -> Calculation<N>;
	/// Convert the operation to a string representation
	fn to_string(&self) -> String;

  fn get_op(&self) -> String;
  fn get_args(&self) -> Vec<Rc<Term<N>>>;
}

#[derive(Debug, Clone)]
/// Represents addition
pub struct Add<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// left arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Add<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		a.op(&b, |a, b| {
			a.add(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({} + {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "+".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> {
    return vec!(Rc::new(self.a.clone()),Rc::new(self.b.clone()));
  }
}

#[derive(Debug, Clone)]
/// Represents subtraction
pub struct Sub<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// right arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Sub<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		a.op(&b, |a, b| {
			a.sub(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({} - {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "-".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> {
    return vec!(Rc::new(self.a.clone()),Rc::new(self.b.clone()));
  }
}

#[derive(Debug, Clone)]
/// Represents multiplication
pub struct Mul<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// right arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Mul<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		a.op(&b, |a, b| {
			a.mul(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({} × {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "*".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> {
    return vec!(Rc::new(self.a.clone()),Rc::new(self.b.clone()));
  }
}

#[derive(Debug, Clone)]
/// Represents division
pub struct Div<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// right arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Div<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		a.op(&b, |a, b| {
			a.div(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({} ÷ {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "/".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> {
    return vec!(Rc::new(self.a.clone()),Rc::new(self.b.clone()));
  }
}

#[derive(Debug, Clone)]
/// Represents 'raised to power'
pub struct Pow<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// right arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Pow<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		a.op(&b, |a, b| {
			a.pow(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({} ^ {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "^".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> {
    return vec!(Rc::new(self.a.clone()),Rc::new(self.b.clone()));
  }
}

#[derive(Debug, Clone)]
/// Represents 'plus or minus'
pub struct PlusMinus<N: Num> {
  /// left arg
	pub a: Term<N>,
  /// right arg
	pub b: Term<N>,
}

impl<N: Num + 'static> Operate<N> for PlusMinus<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;
		let b = self.b.eval_ctx(ctx)?;

		let adds = a.op(&b, |a, b| {
			a.add(b, ctx)
		})?;
		let subs = a.op(&b, |a, b| {
			a.sub(b, ctx)
		})?;

		Ok(adds.join(subs))
	}

	fn to_string(&self) -> String {
		format!("({} ± {})", self.a, self.b)
	}

  fn get_op(&self) -> String { return "±".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}

#[derive(Debug, Clone)]
/// Represents negation
pub struct Neg<N: Num> {
  /// arg
	pub a: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Neg<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;

		a.op(&N::from_f64(-1.0, ctx)?, |a, b| {
			a.mul(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("(-{})", self.a)
	}

  fn get_op(&self) -> String { return "-".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}

#[derive(Debug, Clone)]
/// Represents prefix +
pub struct Pos<N: Num> {
  /// arg
	pub a: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Pos<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;

		Ok(a)
	}

	fn to_string(&self) -> String {
		format!("(+{})", self.a)
	}

  fn get_op(&self) -> String { return "+".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}

#[derive(Debug, Clone)]
/// Represents plus or minus
pub struct PosNeg<N: Num> {
  /// arg
	pub a: Term<N>,
}

impl<N: Num + 'static> Operate<N> for PosNeg<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;

		a.unop(|a| {
			let pos = a;
			let neg = a.mul(&N::from_f64(-1.0, ctx)?.unwrap_single(), ctx)?;

			Ok(neg.join(Answer::Single(pos.clone())))
		})
	}

	fn to_string(&self) -> String {
		format!("(±{})", self.a)
	}

  fn get_op(&self) -> String { return "±".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}

#[derive(Debug, Clone)]
/// Represents factorial
pub struct Fact<N: Num> {
  /// arg
	pub a: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Fact<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		Err(MathError::Unimplemented {
			op: "Factorial".to_string(),
			num_type: "Any".to_string(),
		})
	}

	fn to_string(&self) -> String {
		format!("({}!)", self.a)
	}

  fn get_op(&self) -> String { return "!".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}

#[derive(Debug, Clone)]
/// Represents 'as a percentage'
pub struct Percent<N: Num> {
  /// arg
	pub a: Term<N>,
}

impl<N: Num + 'static> Operate<N> for Percent<N> {
	fn eval(&self, ctx: &Context<N>) -> Calculation<N> {
		let a = self.a.eval_ctx(ctx)?;

		a.op(&N::from_f64(-0.01, ctx)?, |a, b| {
			a.mul(b, ctx)
		})
	}

	fn to_string(&self) -> String {
		format!("({}%)", self.a)
	}

  fn get_op(&self) -> String { return "%".to_string(); }
  fn get_args(&self) -> Vec<Rc<Term<N>>> { return vec!(Rc::new(self.a.clone())); }
}
