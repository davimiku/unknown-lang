use std::fmt::{self, Display};
use std::io;
use std::ops::Deref;

use hir::{Context, ContextDisplay};
use itertools::Itertools;
use la_arena::Idx;

use crate::syntax::{
    BinOp, Constant, Mutability, Operand, Place, Rvalue, Statement, SwitchIntTargets, Terminator,
    UnOp,
};
use crate::{BasicBlock, Function, Local, Program};

type Indent = usize;
const INDENT_SIZE: usize = 4;

pub trait MirWrite {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()>;
}

impl MirWrite for Program {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        for function in self.functions.values() {
            function.write(buf, context, indent)?;
            write_line(buf, indent)?;
        }
        Ok(())
    }
}

impl MirWrite for Function {
    /// Example function display for something that calculates the
    /// distance between two points.
    ///
    /// ```txt
    /// fun distance:
    ///   params: _1, _2
    ///   mut _0: Float
    ///   _1: Point
    ///   _2: Point
    ///
    ///   BB0:
    ///     <see the BasicBlock documentation for an example>
    ///
    ///   BB1:
    ///     Return _0 ->
    /// ```
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        write_signature(buf, self, context, indent)?;

        write_locals(buf, self, context, indent)?;

        // TODO: debug & scope information
        write_basic_blocks(buf, self, context, indent)?;

        write_line_and_dedent(buf, indent)
    }
}

impl Function {}

fn write_line<W: io::Write>(buf: &mut W, indent: &mut Indent) -> io::Result<()> {
    writeln!(buf)?;
    write!(buf, "{}", " ".repeat(*indent * INDENT_SIZE))
}

fn write_line_and_indent<W: io::Write>(buf: &mut W, indent: &mut Indent) -> io::Result<()> {
    *indent += 1;
    write_line(buf, indent)
}

fn write_line_and_dedent<W: io::Write>(buf: &mut W, indent: &mut Indent) -> io::Result<()> {
    if *indent > 0 {
        *indent -= 1;
    }
    write_line(buf, indent)
}

fn write_signature<W: io::Write>(
    buf: &mut W,
    function: &Function,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    let function_name = function
        .name
        .map_or("{anonymous}", |(key, ..)| context.lookup(key));
    write!(buf, "fun {function_name}:")?;
    write_line_and_indent(buf, indent)?;

    write!(buf, "params: ")?;
    if function.params.is_empty() {
        write!(buf, "{{none}}")?;
    }
    let len = function.params.len();
    for (i, _) in function.params.iter().enumerate() {
        write!(buf, "_{}", i + 1)?;
        if i < len - 1 {
            write!(buf, ", ")?;
        }
    }
    write_line(buf, indent)
}

fn write_locals<W: io::Write>(
    buf: &mut W,
    function: &Function,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    for (i, local) in function.locals.iter() {
        let i = i.into_raw().into_u32();
        let ty = local.ty.display(context);

        let mutability = local.mutability;
        write!(buf, "{mutability}_{i}: {ty}")?;
        write_line(buf, indent)?;
    }
    Ok(())
}

fn write_basic_blocks<W: io::Write>(
    buf: &mut W,
    function: &Function,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    write_line(buf, indent)?;
    for (i, basic_block) in function.blocks.iter() {
        let params = basic_block
            .parameters
            .iter()
            .map(idx_local_to_string)
            .join(", ");

        let bb = idx_basic_block(i);
        write!(buf, "{bb}({params}):")?;
        write_line_and_indent(buf, indent)?;
        basic_block.write(buf, context, indent)?;
    }
    Ok(())
}

impl MirWrite for Idx<BasicBlock> {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        write!(buf, "{}", &idx_basic_block(*self))
    }
}

fn idx_basic_block(idx: Idx<BasicBlock>) -> String {
    format!("BB{}", idx.into_raw().into_u32())
}

impl MirWrite for BasicBlock {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        for statement in self.statements.iter() {
            statement.write(buf, context, indent)?;
            write_line(buf, indent)?;
        }
        if let Some(terminator) = &self.terminator {
            terminator.write(buf, context, indent)?;
        }
        write_line_and_dedent(buf, indent)?;

        Ok(())
    }
}

impl MirWrite for Statement {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Statement::Assign(b) => {
                let (place, rvalue) = b.deref();
                place.write(buf, context, indent)?;
                write!(buf, " = ")?;
                rvalue.write(buf, context, indent)
            }
            Statement::SetDiscriminant { .. } => todo!(),
            Statement::StorageLive(..) => todo!(),
            Statement::StorageDead(..) => todo!(),
            Statement::Intrinsic(..) => todo!(),
        }
    }
}

impl MirWrite for Terminator {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Terminator::Jump { target } => {
                write!(buf, "Jump -> ")?;
                target.write(buf, context, indent)
            }
            Terminator::Return => write!(buf, "Return _0 ->"),
            Terminator::Call {
                func,
                args,
                destination,
                target,
            } => todo!(),
            Terminator::SwitchInt {
                discriminant,
                targets,
            } => {
                write!(buf, "SwitchInt(")?;
                discriminant.write(buf, context, indent)?;
                write!(buf, "): ");

                targets.write(buf, context, indent)
            }
            Terminator::Drop { place, target } => todo!(),
            Terminator::Unreachable => todo!(),
        }
    }
}

impl MirWrite for SwitchIntTargets {
    fn write<W: io::Write>(&self, buf: &mut W, _: &Context, _: &mut Indent) -> io::Result<()> {
        write!(buf, "[")?;
        let branch_targets = self
            .branches
            .iter()
            .map(|(value, bb)| {
                let bb = idx_basic_block(*bb);
                format!("{value} -> {bb}")
            })
            .join(", ");
        write!(buf, "{branch_targets}")?;
        if let Some(otherwise) = self.otherwise {
            write!(buf, ", ")?;
            let bb = idx_basic_block(otherwise);
            write!(buf, "else -> {bb}")?;
        }
        write!(buf, "]")
    }
}

impl MirWrite for Place {
    fn write<W: io::Write>(&self, buf: &mut W, _: &Context, _: &mut Indent) -> io::Result<()> {
        write!(buf, "{}", idx_local_to_string(&self.local))
        // TODO: write projections, such as _1.2
        //                                    ^^ (field projection)
    }
}

impl MirWrite for Rvalue {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Rvalue::Use(op) => op.write(buf, context, indent),
            Rvalue::BinaryOp(binop, ops) => {
                let (lhs, rhs) = ops.deref();
                write!(buf, "{binop}(")?;
                lhs.write(buf, context, indent)?;
                write!(buf, ", ")?;
                rhs.write(buf, context, indent)?;
                write!(buf, ")")
            }
            Rvalue::UnaryOp(unop, op) => {
                write!(buf, "{unop}(")?;
                op.write(buf, context, indent)?;
                write!(buf, ")")
            }
        }
    }
}

impl MirWrite for Operand {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Operand::Copy(place) => {
                write!(buf, "copy ")?;
                place.write(buf, context, indent)
            }
            Operand::Move(place) => {
                write!(buf, "move ")?;
                place.write(buf, context, indent)
            }
            Operand::Constant(constant) => {
                write!(buf, "const ")?;
                constant.write(buf, context, indent)
            }
        }
    }
}

impl MirWrite for Constant {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        context: &Context,
        _: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Constant::Int(i) => write!(buf, "{i}"),
            Constant::Float(f) => {
                let mut ryu_buf = ryu::Buffer::new();
                write!(buf, "{}", ryu_buf.format_finite(*f))
            }
            Constant::String(key) => write!(buf, "\"{}\"", context.lookup(*key)),
        }
    }
}

impl Display for Mutability {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Mutability::Not => f.write_str(""),
            Mutability::Mut => f.write_str("mut "),
        }
    }
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

// TODO: better way to say "derive Display from Debug" ?
impl Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

fn idx_local_to_string(idx: &Idx<Local>) -> String {
    let mut s = String::with_capacity(3); // most locals should be index 0-99
    s.push('_');
    s.push_str(&idx.into_raw().into_u32().to_string());
    s
}
