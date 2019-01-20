use std::io;

pub struct PrettyFormatter<'a> {
    current_indent: usize,
    indent: &'a str,
    loop_prefix: &'a str,
    labeled_block_prefix: &'a str,
    labeled_block_suffix: Option<&'a str>,
    label_prefix: &'a str,
}

impl<'a> PrettyFormatter<'a> {
    pub fn new() -> Self {
        PrettyFormatter::with_indent("  ")
    }
    pub fn with_indent(indent: &'a str) -> Self {
        PrettyFormatter {
            current_indent: 0,
            indent: indent,
            loop_prefix: "while (true) ",
            labeled_block_prefix: "do ",
            labeled_block_suffix: Some(" while(0)"),
            label_prefix: "L",
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
    fn write_loop_prefix<W: io::Write>(
        &mut self,
        id: usize,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        write!(writer, "{}{}: {}", self.label_prefix, id, self.loop_prefix)?;
        self.write_open_curly(writer)?;

        self.current_indent += 1;
        Ok(())
    }
    fn write_block_prefix<W: io::Write>(
        &mut self,
        id: usize,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        indent(writer, self.current_indent, self.indent)?;
        write!(
            writer,
            "{}{}: {}",
            self.label_prefix, id, self.labeled_block_prefix
        )?;
        self.write_open_curly(writer)?;

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
    fn write_labeled_block_suffix<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        self.current_indent -= 1;

        indent(writer, self.current_indent, self.indent)?;
        let suffix = if let Some(s) = self.labeled_block_suffix {
            s
        } else {
            ""
        };
        writeln!(writer, "{}{}", "}", suffix)?;

        Ok(())
    }
    fn write_open_curly<W: io::Write>(
        &mut self,
        writer: &mut W,
    ) -> Result<(), io::Error> {
        writeln!(writer, "{}", "{")
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
