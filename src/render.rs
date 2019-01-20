use std::convert::AsRef;

use crate::shapes::*;
use crate::types::{BlockId, FlowType, ProcessedBranch, ShapeId};

#[derive(Debug)]
pub enum CondType<C> {
    If(C),
    ElseIf(C),
    IfLabel(BlockId),
    ElseIfLabel(BlockId),
    Else,
}

#[derive(Debug, Copy, Clone)]
pub struct Exit {
    pub set_label: Option<BlockId>,
    pub flow: Flow,
}
#[derive(Debug, Copy, Clone)]
pub enum Flow {
    Direct,
    Break(Option<ShapeId>),
    Continue(Option<ShapeId>),
}

pub trait StructedAst {
    type Expr: ?Sized;

    fn trap() -> Self;

    fn expr(expr: &Self::Expr) -> Self;

    fn exit(b: Exit) -> Self;

    fn join(self, other: Self) -> Self;

    fn wrap_in_loop(self, shape_id: ShapeId) -> Self;

    fn wrap_in_block(self, shape_id: ShapeId) -> Self;

    fn handled(self, cond_type: CondType<&Self::Expr>) -> Self;
}

impl<E> Shape<E, E> {
    pub fn render<S: StructedAst>(&self) -> S
    where
        E: AsRef<S::Expr>,
    {
        let head: S = match &self.kind {
            ShapeKind::Simple(s) => s.render(None),
            ShapeKind::Multi(s) => s.render(self.id),
            ShapeKind::Loop(s) => s.render(self.id),
            ShapeKind::Fused(s) => s.render(self.id),
        };
        if let Some(ref next) = self.next {
            head.join(next.render())
        } else {
            head
        }
    }
}
impl<E> SimpleShape<E, E> {
    fn render_branch<S: StructedAst>(
        &self,
        set_label: bool,
        branch: &ProcessedBranch<E>,
        fused_multi: Option<&MultipleShape<E, E>>,
    ) -> S
    where
        E: AsRef<S::Expr>,
    {
        if let Some(handled_shape) =
            fused_multi.and_then(|s| s.handled.get(&branch.target))
        {
            assert!(branch.flow_type == FlowType::Direct);
            assert!(handled_shape.entry_type != EntryType::Checked);
            handled_shape.shape.render()
        } else {
            let exit = match branch.flow_type {
                FlowType::Direct => Exit {
                    set_label: Some(branch.target).filter(|_| set_label),
                    flow: Flow::Direct,
                },
                FlowType::Continue => Exit {
                    set_label: Some(branch.target).filter(|_| set_label),
                    flow: Flow::Continue(Some(branch.ancestor)),
                },
                FlowType::Break => Exit {
                    set_label: Some(branch.target).filter(|_| set_label),
                    flow: Flow::Break(Some(branch.ancestor)),
                },
            };
            S::exit(exit)
        }
    }
    fn render<S: StructedAst>(
        &self,
        fused_multi: Option<&MultipleShape<E, E>>,
    ) -> S
    where
        E: AsRef<S::Expr>,
    {
        let mut output: S = S::expr(self.internal.as_ref());

        for (is_first, b) in flag_first(self.conditional_branches()) {
            let cond = b.data.as_ref().unwrap();
            let cond = cond.as_ref();
            let cond_type: CondType<&S::Expr> = if is_first {
                CondType::If(cond)
            } else {
                CondType::ElseIf(cond)
            };
            let exit: S = self.render_branch(true, b, fused_multi);
            let exit = exit.handled(cond_type);

            output = output.join(exit);
        }

        if let Some(b) = self.default_branch() {
            let has_multiple_targets = self.branches_out.len() > 1;
            let exit: S =
                self.render_branch(has_multiple_targets, b, fused_multi);

            if has_multiple_targets {
                let cond_type: CondType<&S::Expr> = CondType::Else;
                let exit = exit.handled(cond_type);
                output = output.join(exit);
            } else {
                output = output.join(exit);
            }
            // } else {
            // output = output.join(S::trap());
        }

        output
    }
}
impl<E> MultipleShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId) -> S
    where
        E: AsRef<S::Expr>,
    {
        let mut body: Option<S> = None;

        for (is_first, (target, inner_shape)) in flag_first(self.handled.iter())
        {
            let cond_type: CondType<&S::Expr> = if is_first {
                CondType::IfLabel(*target)
            } else {
                CondType::ElseIfLabel(*target)
            };
            let inner: S = inner_shape.shape.render();
            let inner: S = inner.handled(cond_type);
            if body.is_none() {
                body = Some(inner);
            } else {
                body = body.map(|body| body.join(inner));
            }
        }

        if self.break_count > 0 {
            body.unwrap().wrap_in_block(shape_id)
        } else {
            body.unwrap()
        }
    }
}
impl<E> LoopShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId) -> S
    where
        E: AsRef<S::Expr>,
    {
        let inner: S = self.inner.render();
        inner.wrap_in_loop(shape_id)
    }
}
impl<E> FusedShape<E, E> {
    fn render<S: StructedAst>(&self, shape_id: ShapeId) -> S
    where
        E: AsRef<S::Expr>,
    {
        let inner: S = self.simple.render(Some(&self.multi));
        if self.multi.break_count > 0 {
            inner.wrap_in_block(shape_id)
        } else {
            inner
        }
    }
}

fn flag_first<I: Iterator>(iter: I) -> impl Iterator<Item = (bool, I::Item)> {
    iter.enumerate().map(|(i, x)| (i == 0, x))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io;

    #[derive(Debug)]
    enum Ast {
        Panic,
        Node(String),
        If(String, Box<Ast>),
        ElseIf(String, Box<Ast>),
        Else(Box<Ast>),
        Loop(usize, Box<Ast>),
        Block(Option<usize>, Vec<Ast>),
    }

    impl StructedAst for Ast {
        type Expr = str;

        fn trap() -> Self {
            Ast::Panic
        }

        fn expr(expr: &Self::Expr) -> Self {
            Ast::Node(expr.to_string())
        }

        fn exit(exit: Exit) -> Self {
            let label = if let Some(bid) = exit.set_label {
                format!("__label__ = {}\n", bid.index())
            } else {
                "\n".to_string()
            };

            match exit.flow {
                Flow::Direct => Ast::Node(label),
                Flow::Continue(Some(shape)) => {
                    let code = format!("{}continue 'L{}", label, shape.index());
                    Ast::Node(code)
                }
                Flow::Continue(None) => {
                    let code = format!("{}continue", label);
                    Ast::Node(code)
                }
                Flow::Break(Some(shape)) => {
                    let code = format!("{}break 'L{}", label, shape.index());
                    Ast::Node(code)
                }
                Flow::Break(None) => {
                    let code = format!("{}break", label);
                    Ast::Node(code)
                }
            }
        }

        fn join(mut self, other: Self) -> Self {
            match &mut self {
                Ast::Block(None, xs) => {
                    xs.push(other);
                    self
                }
                _ => Ast::Block(None, vec![self, other]),
            }
        }

        fn wrap_in_loop(self, shape_id: ShapeId) -> Self {
            let id = shape_id.index();
            Ast::Loop(id, Box::new(self))
        }

        fn wrap_in_block(self, shape_id: ShapeId) -> Self {
            let id = shape_id.index();
            Ast::Block(Some(id), vec![self])
        }

        fn handled(self, cond_type: CondType<&Self::Expr>) -> Self {
            match cond_type {
                CondType::If(c) => Ast::If(c.to_string(), Box::new(self)),
                CondType::ElseIf(c) => {
                    Ast::ElseIf(c.to_string(), Box::new(self))
                }
                CondType::IfLabel(bid) => Ast::If(
                    format!("__label__ == {}", bid.index()),
                    Box::new(self),
                ),
                CondType::ElseIfLabel(bid) => Ast::ElseIf(
                    format!("__label__ == {}", bid.index()),
                    Box::new(self),
                ),
                CondType::Else => Ast::Else(Box::new(self)),
            }
        }
    }

    impl Ast {
        fn print<W: io::Write>(&self, writer: &mut W) -> Result<(), io::Error> {
            let mut f = PrettyFormatter::new();
            self.fmt(&mut f, writer)
        }
        fn fmt<W: io::Write>(
            &self,
            f: &mut PrettyFormatter,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            match self {
                Ast::Node(s) => f.write_node(s, writer),
                Ast::Panic => f.write_node("unreachable!()", writer),
                Ast::If(s, inner) => {
                    f.write_if(s, writer)?;
                    inner.fmt(f, writer)?;
                    f.write_block_suffix(writer)
                }
                Ast::ElseIf(s, inner) => {
                    f.write_else_if(s, writer)?;
                    inner.fmt(f, writer)?;
                    f.write_block_suffix(writer)
                }
                Ast::Else(inner) => {
                    f.write_else(writer)?;
                    inner.fmt(f, writer)?;
                    f.write_block_suffix(writer)
                }
                Ast::Loop(id, inner) => {
                    f.write_loop_prefix(*id, writer)?;
                    inner.fmt(f, writer)?;
                    f.write_block_suffix(writer)
                }
                Ast::Block(None, inners) => {
                    for inner in inners {
                        inner.fmt(f, writer)?;
                    }
                    Ok(())
                }
                Ast::Block(Some(id), inners) => {
                    f.write_block_prefix(*id, writer)?;
                    for inner in inners {
                        inner.fmt(f, writer)?;
                    }
                    f.write_block_suffix(writer)
                }
            }
        }
    }

    #[derive(Clone, Debug)]
    pub struct PrettyFormatter<'a> {
        current_indent: usize,
        indent: &'a str,
    }

    impl<'a> PrettyFormatter<'a> {
        fn new() -> Self {
            PrettyFormatter::with_indent("  ")
        }
        fn with_indent(indent: &'a str) -> Self {
            PrettyFormatter {
                current_indent: 0,
                indent: indent,
            }
        }
        fn write_node<W: io::Write>(
            &mut self,
            s: &str,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            for line in s.lines() {
                self.write_line(line, writer)?;
            }
            Ok(())
        }
        fn write_line<W: io::Write>(
            &mut self,
            s: &str,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            writeln!(writer, "{}", s)
        }
        fn write_if<W: io::Write>(
            &mut self,
            cond: &str,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            self.current_indent += 1;
            writeln!(writer, "if {} {}", cond, "{")
        }
        fn write_else_if<W: io::Write>(
            &mut self,
            cond: &str,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            self.current_indent += 1;
            writeln!(writer, "else if {} {}", cond, "{")
        }
        fn write_else<W: io::Write>(
            &mut self,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            self.current_indent += 1;
            writeln!(writer, "else {}", "{")
        }
        fn write_loop_prefix<W: io::Write>(
            &mut self,
            id: usize,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            writeln!(writer, "'L{}: loop {}", id, "{")?;

            self.current_indent += 1;
            Ok(())
        }
        fn write_block_prefix<W: io::Write>(
            &mut self,
            id: usize,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            indent(writer, self.current_indent, self.indent)?;
            writeln!(writer, "'L{}: {}", id, "{")?;

            self.current_indent += 1;
            Ok(())
        }
        fn write_block_suffix<W: io::Write>(
            &mut self,
            writer: &mut W,
        ) -> Result<(), io::Error> {
            self.current_indent -= 1;

            indent(writer, self.current_indent, self.indent)?;
            writeln!(writer, "{}", "}")?;

            Ok(())
        }
    }

    fn indent<W: ?Sized>(wr: &mut W, n: usize, s: &str) -> io::Result<()>
    where
        W: io::Write,
    {
        for _ in 0..n {
            wr.write_all(s.as_bytes())?;
        }

        Ok(())
    }

    #[test]
    fn test_it() {
        use crate::relooper::Relooper;
        let mut relooper: Relooper<String, String> = Relooper::new();

        let a = relooper.add_block("x = 0".to_string());
        let b = relooper.add_block("// block b".to_string());
        let c = relooper.add_block("// block c".to_string());
        let d = relooper.add_block("// block d".to_string());

        relooper.add_branch(a, b, Some("a -> b".to_string()));
        relooper.add_branch(a, c, None);
        relooper.add_branch(b, c, None);
        relooper.add_branch(c, d, None);
        relooper.add_branch(b, d, Some("b -> d".to_string()));

        let ast: Ast = relooper.render(a).expect("Did not get shape");

        let stdout = io::stdout();
        {
            let mut out = stdout.lock();
            ast.print(&mut out);
        }

        assert!(false);
    }
}
