mod process;
mod relooper;
mod render;
mod shapes;
mod types;

pub use crate::relooper::Relooper;
pub use crate::render::{CondType, Exit, Flow, StructedAst};
pub use crate::types::{BlockId, ShapeId};

pub mod generic_c_like;

/// # Very BASIC control flows
///
/// These tests operate on generated string representation of pseduo code.
/// They will need to be updated if additional optimization has been added
/// to Relooper, or CLikeAst struct default behavior changes.
#[cfg(test)]
mod test_basics {
    use std::iter::Peekable;

    use super::Relooper;
    use crate::generic_c_like::{AstKind, CLikeAst};

    #[test]
    fn test_basics_simple_x3() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("__block__a".to_string());
        let b = relooper.add_block("__block__b".to_string());
        let c = relooper.add_block("__block__c".to_string());

        // a +---> b +--> c
        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
            __block__a
            __block__b
            __block__c
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_single_loop() {
        let mut relooper: Relooper<String> = Relooper::new();

        let a = relooper.add_block("i = 0".to_string());
        let b = relooper.add_block("__block__b".to_string());
        let c = relooper.add_block("i = i + 1".to_string());
        let d = relooper.add_block("__block__d".to_string());

        //       +--------+
        //       |        |
        //       v        |
        // a +-> b +----->c
        //       +
        //       |
        //       v
        //       d
        relooper.add_branch(a, b, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(b, d, Some("i > 10".to_string()));
        relooper.add_branch(c, b, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
	    i = 0
	    L1: while (true) {
	      __block__b
	      if ( i > 10 ) {
		break L1;
	      }
	      i = i + 1
	      continue L1;
	    }
	    __block__d
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_if_else() {
        let mut relooper: Relooper<&'static str> = Relooper::new();

        let a = relooper.add_block("__block__a");
        let b = relooper.add_block("__block__b");
        let c = relooper.add_block("__block__c");

        relooper.add_branch(a, b, Some("true"));
        relooper.add_branch(a, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
	    __block__a
	    if ( true ) {
	      __block__b
	    }
	    else {
	      __block__c
	    }
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    #[test]
    fn test_basics_if() {
        let mut relooper: Relooper<&'static str> = Relooper::new();

        let a = relooper.add_block("__block__a");
        let b = relooper.add_block("__block__b");
        let c = relooper.add_block("__block__c");

        // a+---------->c
        // |            ^
        // |            |
        // +----->b+----+
        relooper.add_branch(a, b, Some("true"));
        relooper.add_branch(a, c, None);
        relooper.add_branch(b, c, None);

        let ast: CLikeAst = relooper.render(a).expect("Did not get shape");

        let pseudo_code = ast.to_string();
        let expected = "
	    L1: do {
	      __block__a
	      if ( true ) {
		__block__b
		break L1;
	      }
	      else {
		break L1;
	      }
	    } while(false)
	    __block__c
        ";

        assert!(
            code_equal(pseudo_code.trim().chars(), expected.trim().chars()),
            pseudo_code
        );
    }

    // replace consecutive white spaces with a single space
    struct InsignificantWhites<I: Iterator> {
        chars: Peekable<I>,
    }
    impl<I: Iterator<Item = char>> InsignificantWhites<I> {
        #[allow(dead_code)]
        fn new(chars: I) -> Self {
            InsignificantWhites {
                chars: chars.peekable(),
            }
        }
    }
    impl<I: Iterator<Item = char>> Iterator for InsignificantWhites<I> {
        type Item = char;

        fn next(&mut self) -> Option<char> {
            let mut ret = None;
            while let Some(c) = self.chars.peek() {
                if !c.is_whitespace() {
                    if ret.is_none() {
                        ret = self.chars.next();
                    }
                    break;
                }

                ret = Some(' ');
                self.chars.next();
            }

            ret
        }
    }

    // Generic c like AST is used for unit test purposes
    // generally, white spaces are insignificant in this
    // style of pseudo code, by comparing generated pseudo
    // code, we can ensure relooper produces rightly "shaped"
    // code.
    fn code_equal<A, B>(x: A, y: B) -> bool
    where
        A: IntoIterator<Item = char>,
        B: IntoIterator<Item = char>,
    {
        let mut x = InsignificantWhites {
            chars: x.into_iter().peekable(),
        };
        let mut y = InsignificantWhites {
            chars: y.into_iter().peekable(),
        };
        loop {
            let c1 = x.next();
            let c2 = y.next();
            match (c1, c2) {
                (Some(c1), Some(c2)) if c1 == c2 => continue,
                (None, None) => return true,
                _ => return false,
            }
        }
    }

}
