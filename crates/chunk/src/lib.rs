// Takes an HIR node + maps of values & types, produces a
// bytecode chunk (vector of bytes)

mod op;
mod stack;

use op::Idx;
pub use op::Op;
pub use stack::ValueStack;

use hir::{Database, Expr, LocalDef, Stmt};

#[derive(Debug, Default)]
pub struct Chunk<'a> {
    code: Vec<Op>,
    int_constants: Vec<i64>,
    float_constants: Vec<f64>,
    str_constants: Vec<&'a str>,
    lines: Vec<u32>,
}

// TODO: make all pub(crate)
impl<'a> Chunk<'a> {
    pub fn new() -> Chunk<'a> {
        Default::default()
    }

    #[inline(always)]
    pub fn get_op(&self, i: usize) -> &Op {
        &self.code[i]
    }

    // TODO: test this when the VM is stable
    #[inline(always)]
    #[cfg(not(debug_assertions))]
    pub fn get_int(&self, i: Idx) -> i64 {
        unsafe { *self.int_constants.get_unchecked(i) }
    }

    // panics if index out-of-bounds
    #[inline(always)]
    #[cfg(debug_assertions)]
    pub fn get_int(&self, i: Idx) -> i64 {
        self.int_constants[i as usize]
    }

    // TODO: test this when the VM is stable
    #[inline(always)]
    #[cfg(not(debug_assertions))]
    pub fn get_float(&self, i: Idx) -> f64 {
        unsafe { *self.float_constants.get_unchecked(i) }
    }

    // panics if index out-of-bounds
    #[inline(always)]
    #[cfg(debug_assertions)]
    pub fn get_float(&self, i: Idx) -> f64 {
        self.float_constants[i as usize]
    }

    // panics if index out-of-bounds
    #[inline(always)]
    #[cfg(not(debug_assertions))]
    pub fn get_str(&self, i: Idx) -> &str {
        unsafe { *self.str_constants.get_unchecked(i) }
    }

    // panics if index out-of-bounds
    #[inline(always)]
    #[cfg(debug_assertions)]
    pub fn get_str(&self, i: Idx) -> &str {
        self.str_constants[i as usize]
    }

    pub fn write(&mut self, op: Op, line: u32) {
        self.code.push(op);
        self.lines.push(line);
    }

    pub fn write_int_constant(&mut self, value: i64, line: u32) {
        self.int_constants.push(value);
        let i = self.int_constants.len() - 1;
        self.write(Op::IConstant(i as u32), line);
    }

    pub fn write_float_constant(&mut self, value: f64, line: u32) {
        self.float_constants.push(value);
        let i = self.float_constants.len() - 1;
        self.write(Op::FConstant(i as u32), line);
    }

    pub fn write_bool_constant(&mut self, value: bool, line: u32) {
        match value {
            true => self.write(Op::TrueConstant, line),
            false => self.write(Op::FalseConstant, line),
        }
    }

    pub fn write_string_constant(&mut self, value: &'a str, line: u32) {
        self.str_constants.push(value);
        let i = self.str_constants.len() - 1;

        self.write(Op::SConstant(i as u32), line);
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        for (i, op) in self.code.iter().enumerate() {
            let line = self.lines[i];
            // TODO: print a "  | " if it has the same line as the previous Op
            println!("{:03} {} {}", i, line, op.disassemble(self))
        }
    }

    pub fn write_stmt(&mut self, stmt: Stmt, database: &Database) {
        match stmt {
            Stmt::VariableDef(local_def) => todo!(),
            Stmt::Expr(idx) => {
                let expr = database.expr(idx);
                self.write_expr(expr, database)
            }
        }
    }

    pub fn write_expr(&mut self, expr: &Expr, database: &Database) {
        use Expr::*;

        match expr {
            BoolLiteral(b) => {
                self.write_bool_constant(*b, 1);
            }
            FloatLiteral(f) => {
                self.write_float_constant(*f, 1);
            }
            IntLiteral(i) => {
                self.write_int_constant(*i, 1);
            }
            StringLiteral(s) => todo!(),
            Binary {
                op,
                lhs,
                rhs,
                lhs_type,
                rhs_type,
            } => {
                //

                match op {
                    hir::BinaryOp::Add => {

                        // write push lhs (expr)
                        // write push rhs (expr)
                        // write Add or FAdd depending on type
                    }
                    hir::BinaryOp::Sub => todo!(),
                    hir::BinaryOp::Mul => todo!(),
                    hir::BinaryOp::Div => todo!(),
                    hir::BinaryOp::Rem => todo!(),
                    hir::BinaryOp::Exp => todo!(),
                    hir::BinaryOp::Path => todo!(),
                }
            }
            Unary { op, expr, typ } => todo!(),
            Block { stmts, typ } => todo!(),
            VariableRef { name, typ } => todo!(),
            Function {
                params,
                body,
                return_type_annotation,
            } => todo!(),
            Empty => todo!(),
        }
    }

    fn write_local_def(&mut self, local_def: &LocalDef) {
        let name = local_def.name();
        let value = local_def.value;
    }
}

#[cfg(test)]
mod tests {}
