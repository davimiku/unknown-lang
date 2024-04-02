use std::fmt::{self, Display, Write};
use std::io;
use std::ops::Deref;

use hir::{Context, ContextDisplay};
use itertools::Itertools;
use la_arena::Idx;

use crate::syntax::{
    BinOpKind, BlockTarget, BranchIntTargets, Constant, Operand, Place, Rvalue, Statement,
    Terminator, UnOp,
};
use crate::{BasicBlock, Function, Local, Module};

type Indent = usize;
const INDENT_SIZE: usize = 4;

pub trait MirWrite {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()>;
}

pub fn write_module<W: io::Write>(
    module: &Module,
    buf: &mut W,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    for function in module.functions.values() {
        function.write(buf, module, context, indent)?;
    }
    Ok(())
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
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        write_signature(buf, self, module, context, indent)?;

        write_locals_list(buf, self, module, context, indent)?;

        // TODO: debug & scope information
        write_basic_blocks(buf, self, module, context, indent)?;

        write_line_and_dedent(buf, indent)
    }
}

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
    _: &Module,
    _: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    let function_name: &str = function.name.as_deref().unwrap_or("{anonymous}");
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

fn write_locals_list<W: io::Write>(
    buf: &mut W,
    function: &Function,
    _: &Module,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    for (idx, local) in function.locals.iter() {
        let ty = local.ty.display(context);

        let mutability = local.mutability;
        let local = idx_local_to_string(&idx);
        write!(buf, "{mutability}{local}: {ty}")?;
        write_line(buf, indent)?;
    }
    Ok(())
}

fn write_basic_blocks<W: io::Write>(
    buf: &mut W,
    function: &Function,
    module: &Module,
    context: &Context,
    indent: &mut Indent,
) -> io::Result<()> {
    write_line(buf, indent)?;
    for (idx, basic_block) in function.blocks.iter() {
        if !function.predecessors.has_predecessors(idx) {
            continue;
        }
        let params = basic_block
            .parameters
            .iter()
            .map(idx_local_to_string)
            .join(", ");

        let bb = idx_basic_block_to_string(idx);
        write!(buf, "{bb}({params}):")?;
        write_line_and_indent(buf, indent)?;
        basic_block.write(buf, module, context, indent)?;
    }
    Ok(())
}

impl MirWrite for Idx<BasicBlock> {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        _: &Module,
        _context: &Context,
        _indent: &mut Indent,
    ) -> io::Result<()> {
        write!(buf, "{}", &idx_basic_block_to_string(*self))
    }
}

impl Display for BlockTarget {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(&idx_basic_block_to_string(self.target))?;
        f.write_char('(')?;
        f.write_str(&self.args.iter().map(idx_local_to_string).join(", "))?;
        f.write_char(')')
    }
}

impl MirWrite for BasicBlock {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        for statement in self.statements.iter() {
            statement.write(buf, module, context, indent)?;
            write_line(buf, indent)?;
        }
        if let Some(terminator) = &self.terminator {
            terminator.write(buf, module, context, indent)?;
        }
        write_line_and_dedent(buf, indent)?;

        Ok(())
    }
}

impl MirWrite for Statement {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Statement::Assign(b) => {
                let (place, rvalue) = b.deref();
                place.write(buf, module, context, indent)?;
                write!(buf, " := ")?;
                rvalue.write(buf, module, context, indent)
            }
            Statement::ReAssign(b) => {
                let (place, rvalue) = b.deref();
                place.write(buf, module, context, indent)?;
                write!(buf, " <- ")?;
                rvalue.write(buf, module, context, indent)
            }
            Statement::SetDiscriminant { .. } => todo!(),
            Statement::StorageLive(..) => todo!(),
            Statement::StorageDead(..) => todo!(),
            Statement::Intrinsic(..) => todo!(),
        }
    }
}

impl MirWrite for BlockTarget {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        _: &Module,
        _: &Context,
        _: &mut Indent,
    ) -> io::Result<()> {
        write!(buf, "{self}")
    }
}

impl MirWrite for Terminator {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Terminator::Jump(block_target) => {
                write!(buf, "Jump -> ")?;
                block_target.write(buf, module, context, indent)
            }
            Terminator::Return => write!(buf, "Return _0 ->"),
            Terminator::Call {
                func,
                args,
                destination,
                target,
            } => {
                destination.write(buf, module, context, indent)?;
                write!(buf, " = ")?;
                func.write(buf, module, context, indent)?;
                write!(buf, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    arg.write(buf, module, context, indent)?;
                    if i < args.len() - 1 {
                        write!(buf, ", ")?;
                    }
                }
                write!(buf, ") -> [return: ")?;
                target
                    .as_ref()
                    .unwrap()
                    .write(buf, module, context, indent)?;

                write!(buf, ", unwind -> TODO]")
            }
            Terminator::BranchInt {
                discriminant,
                targets,
            } => {
                write!(buf, "BranchInt(")?;
                discriminant.write(buf, module, context, indent)?;
                write!(buf, "): ")?;

                targets.write(buf, module, context, indent)
            }
            Terminator::Drop { .. } => todo!(),
            Terminator::Unreachable => todo!(),
        }
    }
}

impl MirWrite for BranchIntTargets {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        write!(buf, "[")?;
        let branch_targets = self
            .branches
            .iter()
            .map(|(value, block_target)| {
                let mut temp_buf = Vec::new();
                let _ = block_target.write(&mut temp_buf, module, context, indent);
                let s = String::from_utf8(temp_buf).unwrap();
                format!("{value} -> {s}")
            })
            .join(", ");
        write!(buf, "{branch_targets}")?;
        if let Some(block_target) = &self.otherwise {
            write!(buf, ", ")?;
            write!(buf, "else -> ")?;
            block_target.write(buf, module, context, indent)?;
        }
        write!(buf, "]")
    }
}

impl MirWrite for Place {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        _: &Module,
        _: &Context,
        _: &mut Indent,
    ) -> io::Result<()> {
        write!(buf, "{}", idx_local_to_string(&self.local))
        // TODO: write projections, such as _1.2
        //                                    ^^ (field projection)
    }
}

impl MirWrite for Rvalue {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Rvalue::Use(op) => op.write(buf, module, context, indent),
            Rvalue::BinaryOp(binop, ops) => {
                let (lhs, rhs) = ops.deref();
                write!(buf, "{binop}(")?;
                lhs.write(buf, module, context, indent)?;
                write!(buf, ", ")?;
                rhs.write(buf, module, context, indent)?;
                write!(buf, ")")
            }
            Rvalue::UnaryOp(unop, op) => {
                write!(buf, "{unop}(")?;
                op.write(buf, module, context, indent)?;
                write!(buf, ")")
            }
        }
    }
}

impl MirWrite for Operand {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        indent: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Operand::Copy(place) => {
                write!(buf, "copy ")?;
                place.write(buf, module, context, indent)
            }
            Operand::Move(place) => {
                write!(buf, "move ")?;
                place.write(buf, module, context, indent)
            }
            Operand::Constant(constant) => {
                if !matches!(constant, Constant::Func(_)) {
                    write!(buf, "const ")?;
                }
                constant.write(buf, module, context, indent)
            }
        }
    }
}

impl MirWrite for Constant {
    fn write<W: io::Write>(
        &self,
        buf: &mut W,
        module: &Module,
        context: &Context,
        _: &mut Indent,
    ) -> io::Result<()> {
        match self {
            Constant::Int(i) => write!(buf, "{i}"),
            Constant::Float(f) => {
                let mut ryu_buf = ryu::Buffer::new();
                write!(buf, "{}", ryu_buf.format(*f))
            }
            Constant::String(key) => write!(buf, "\"{}\"", context.lookup(*key)),
            Constant::Func(func_id) => {
                let name = module.function_names[func_id]
                    .clone()
                    .unwrap_or_else(|| func_id.to_string());
                write!(buf, "{name} ")
            }
        }
    }
}

impl Display for BinOpKind {
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

fn idx_basic_block_to_string(idx: Idx<BasicBlock>) -> String {
    format!("BB{}", idx.into_raw().into_u32())
}

fn idx_local_to_string(idx: &Idx<Local>) -> String {
    let mut s = String::with_capacity(3); // most locals should be index 0-99
    s.push('_');
    s.push_str(&idx.into_raw().into_u32().to_string());
    s
}
