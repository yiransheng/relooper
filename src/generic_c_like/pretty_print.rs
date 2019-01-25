use std::fmt;
use std::io;

use super::{AstConfig, AstKind, CLikeAst, StaticAstConfig};

impl<C: StaticAstConfig> CLikeAst<C> {
    pub fn to_writer_pretty<W: io::Write>(
        &self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        self.to_writer_pretty_indented(0, writer)
    }
    pub fn to_writer_pretty_indented<W: io::Write>(
        &self,
        level: usize,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        let config = C::config();
        let mut pretty_formatter =
            PrettyFormatter::with_indent(level, "  ", config);
        self.kind.pretty_fmt(&mut pretty_formatter, writer)
    }
}

impl<C: StaticAstConfig> fmt::Display for CLikeAst<C> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.to_writer_pretty(&mut FmtWriter { inner: f })
            .map_err(|_| fmt::Error::default())
    }
}

// this is weird...
struct FmtWriter<'a, W> {
    inner: &'a mut W,
}
impl<'a, W: fmt::Write> FmtWriter<'a, W> {
    fn io_error(s: &'static str) -> io::Error {
        io::Error::new(io::ErrorKind::Other, s)
    }
}
impl<'a, W: fmt::Write> io::Write for FmtWriter<'a, W> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let n = buf.len();
        let s = ::std::str::from_utf8(buf)
            .map_err(|_| Self::io_error("Utf8 Error"))?;
        self.inner
            .write_str(s)
            .map_err(|_| Self::io_error("fmt Error"))?;

        Ok(n)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl AstKind {
    fn is_empty_node(&self) -> bool {
        if let AstKind::Seq(ref xs) = self {
            xs.is_empty()
        } else {
            false
        }
    }
    fn pretty_fmt<W: io::Write>(
        &self,
        f: &mut PrettyFormatter,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        match self {
            AstKind::Node(s) => f.write_node(s, writer),
            AstKind::Panic => f.write_node(f.config.panic, writer),
            AstKind::If(s, inner) => {
                f.write_if(s, writer)?;
                inner.pretty_fmt(f, writer)?;
                f.write_block_postfix(writer)
            }
            AstKind::ElseIf(s, inner) => {
                f.write_else_if(s, writer)?;
                inner.pretty_fmt(f, writer)?;
                f.write_block_postfix(writer)
            }
            AstKind::Else(inner) => {
                if !inner.is_empty_node() {
                    f.write_else(writer)?;
                    inner.pretty_fmt(f, writer)?;
                    f.write_block_postfix(writer)
                } else {
                    Ok(())
                }
            }
            AstKind::Loop(id, inner) => {
                f.write_loop_prefix(*id, writer)?;
                inner.pretty_fmt(f, writer)?;
                f.write_loop_postfix(writer)
            }
            AstKind::Seq(inners) => {
                for inner in inners {
                    inner.pretty_fmt(f, writer)?;
                }
                Ok(())
            }
            AstKind::LabeledBlock(id, inner) => {
                f.write_labeled_block_prefix(*id, writer)?;
                inner.pretty_fmt(f, writer)?;
                f.write_labeled_block_postfix(writer)
            }
        }
    }
}

struct PrettyFormatter<'a> {
    current_indent: usize,
    indent: &'a str,
    config: AstConfig<'a>,
}

impl<'a> PrettyFormatter<'a> {
    fn with_indent(
        level: usize,
        indent: &'a str,
        config: AstConfig<'a>,
    ) -> Self {
        PrettyFormatter {
            current_indent: level,
            indent: indent,
            config,
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
        writeln!(writer, "if ( {} ) {}", cond, "{")
    }
    fn write_else_if<W: io::Write>(
        &mut self,
        cond: &str,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        self.current_indent += 1;
        writeln!(writer, "else if ( {} ) {}", cond, "{")
    }
    fn write_else<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        self.current_indent += 1;
        writeln!(writer, "else {}", "{")
    }
    fn write_block_postfix<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        self.current_indent -= 1;

        indent(writer, self.current_indent, self.indent)?;
        writeln!(writer, "{}", "}")?;

        Ok(())
    }
    fn write_loop_prefix<W: io::Write>(
        &mut self,
        id: usize,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        writeln!(
            writer,
            "{}{}: {}",
            self.config.label_prefix, id, self.config.loop_prefix
        )?;

        self.current_indent += 1;
        Ok(())
    }
    fn write_labeled_block_prefix<W: io::Write>(
        &mut self,
        id: usize,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        writeln!(
            writer,
            "{}{}: {}",
            self.config.label_prefix, id, self.config.labed_block_prefix
        )?;

        self.current_indent += 1;
        Ok(())
    }
    fn write_loop_postfix<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        self.current_indent -= 1;

        indent(writer, self.current_indent, self.indent)?;
        writeln!(writer, "{}", self.config.loop_postfix)?;

        Ok(())
    }
    fn write_labeled_block_postfix<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        self.current_indent -= 1;

        indent(writer, self.current_indent, self.indent)?;
        writeln!(writer, "{}", self.config.labed_block_postfix)?;

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
