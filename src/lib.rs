mod generic_c_like;
mod process;
mod relooper;
mod render;
mod shapes;
mod types;

pub use relooper::Relooper;
pub use render::{CondType, Exit, Flow, StructedAst};
pub use types::{BlockId, ShapeId};

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
