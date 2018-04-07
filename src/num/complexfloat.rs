use std::fmt;
use std::cmp::Ordering;

use opers::Calculation;
use num::Num;
use answer::Answer;
use errors::MathError;
use context::Context;

/// A complex number made of a real `f64` and an imaginary `f64`.
#[derive(Debug, Clone)]
pub struct ComplexFloat {
	/// The real part
	pub r: f64,
	/// The imaginary part
	pub i: f64,
}

impl Num for ComplexFloat {
	fn from_f64(t: f64, ctx: &Context<Self>) -> Calculation<Self> {
		Ok(Answer::Single(ComplexFloat {
			r: t,
			i: 0.0,
		}))
	}
	
	fn from_f64_complex((r, i): (f64, f64), ctx: &Context<Self>) -> Calculation<Self> {
		Ok(Answer::Single(ComplexFloat { r, i }))
	}
	
	fn tryord(&self, other: &Self, ctx: &Context<Self>) -> Result<Ordering, MathError> {
		if let Some(ord) = self.partial_cmp(other) {
			Ok(ord)
		} else {
			Err(MathError::CmpError)
		}
	}
	
	fn add(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = self.r + other.r;
		let i = self.i + other.i;
		
		Ok(Answer::Single(ComplexFloat { r, i }))
	}
	
	fn sub(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r = self.r - other.r;
		let i = self.i - other.i;
		
		Ok(Answer::Single(ComplexFloat { r, i }))
	}
	
	fn mul(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let r1 = self.r * other.r;
		let i1 = self.r * other.i;
		let i2 = self.i * other.r;
		let r2 = self.i * other.i;
		let r = r1 - r2;
		let i = i1 + i2;
		
		Ok(Answer::Single(ComplexFloat { r, i }))
	}
	
	fn div(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		let conj = other.conjugate();
		let num = match self.mul(&conj, ctx)? {
			Answer::Single(n) => n,
			Answer::Multiple(_) => unreachable!(),
		};
		let den = match other.mul(&conj, ctx)? {
			Answer::Single(n) => n,
			Answer::Multiple(_) => unreachable!(),
		};
		let r = num.r / den.r;
		let i = num.i / den.r;
		
		Ok(Answer::Single(ComplexFloat { r, i }))
	}
	
	fn pow(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn sqrt(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn nrt(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn abs(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn sin(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn cos(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn tan(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn asin(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn acos(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn atan(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn atan2(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn floor(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn ceil(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn round(&self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
	
	fn log(&self, other: &Self, ctx: &Context<Self>) -> Calculation<Self> {
		unimplemented!()
	}
}

impl ComplexFloat {
	/// Returns the complex conjugate of this number
	pub fn conjugate(&self) -> Self {
		ComplexFloat {
			r: self.r,
			i: -self.i,
		}
	}
}

impl From<(f64, f64)> for ComplexFloat {
	fn from((r, i): (f64, f64)) -> Self {
		ComplexFloat {
			r,
			i,
		}
	}
}

impl From<f64> for ComplexFloat {
	fn from(t: f64) -> Self {
		ComplexFloat {
			r: t,
			i: 0.0,
		}
	}
}

impl PartialOrd for ComplexFloat {
	fn partial_cmp(&self, other: &ComplexFloat) -> Option<Ordering> {
		self.r.partial_cmp(&other.r)
	}
}

impl PartialEq for ComplexFloat {
	fn eq(&self, other: &ComplexFloat) -> bool {
		self.r.eq(&other.r)
	}
}

impl fmt::Display for ComplexFloat {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.i == 0.0 {
			write!(f, "{}", self.r)
		} else if self.r == 0.0 {
			write!(f, "{}i", self.i)
		} else {
			write!(f, "({} + {}i)", self.r, self.i)
		}
	}
}