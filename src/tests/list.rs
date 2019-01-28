use std::rc::Rc;

pub struct List<T> {
    inner: Vec<T>,
}

impl<T> List<T> {
    fn new() -> Self {
        unimplemented!()
    }

    fn merge(&mut self, other: List<T>) {}
}
