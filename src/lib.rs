//! A pure Rust implementation of [`Emscripten Relooper Algorithm`].
//!
//! - [`generic_c_like`](./generic_c_like/index.html) provides a string based AST format which could be used to generate
//! JavaScript source code.
//!
//! # Example
//!
//! ```rust
//! use relooper::*;
//! use relooper::generic_c_like::CLikeAst;
//! let mut relooper: Relooper<String> = Relooper::new();
//!
//! let a = relooper.add_block("i = 0".to_string());
//! let b = relooper.add_block("__block__b".to_string());
//! let c = relooper.add_block("i = i + 1".to_string());
//! let d = relooper.add_block("__block__d".to_string());
//!
//! relooper.add_branch(a, b, None);
//! relooper.add_branch(b, c, None);
//! relooper.add_branch(b, d, Some("i > 10".to_string()));
//! relooper.add_branch(c, b, None);
//!
//! let ast: CLikeAst = relooper.render(a).expect("Did not get shape");
//!
//! println!("{}", ast);
//!
//! ```
//!
//! prints:
//!
//! ```javascript
//! i = 0
//! L1: while (true) {
//!   __block__b
//!   if ( i > 10 ) {
//!     break L1;
//!   }
//!   i = i + 1
//!   continue L1;
//! }
//! __block__d
//! ```
//!
//!
//!
//! [`Emscripten Relooper Algorithm`]: https://github.com/emscripten-core/emscripten/blob/incoming/docs/paper.pdf

mod process;
mod relooper;
mod render;
mod shapes;
mod types;

#[cfg(test)]
mod tests;

pub use crate::relooper::Relooper;
pub use crate::render::{CondType, Exit, Flow, StructuredAst};
pub use crate::types::{BlockId, ShapeId};

pub mod generic_c_like;
