use la_arena::Idx;

use crate::{lowering_context::ContextDisplay, Context, COMPILER_BRAND};

use super::{TypeExpr, TypeRefExpr, TypeSymbol};

impl ContextDisplay for Idx<TypeExpr> {
    fn display(&self, context: &Context) -> String {
        let type_expr = context.type_expr(*self);
        type_expr.display(context)
    }
}

impl ContextDisplay for TypeExpr {
    fn display(&self, context: &Context) -> String {
        match self {
            TypeExpr::Empty => "{{empty}}".to_owned(),

            TypeExpr::BoolLiteral(b) => b.to_string(),
            TypeExpr::FloatLiteral(f) => f.to_string(),
            TypeExpr::IntLiteral(i) => i.to_string(),
            TypeExpr::StringLiteral(key) => format!(r#""{}""#, context.lookup(*key)),
            TypeExpr::Call(_) => todo!(),

            TypeExpr::LocalDef(_) => todo!(),
            TypeExpr::VarRef(type_ref) => type_ref.display(context),
            TypeExpr::UnresolvedVarRef { .. } => todo!(),
            TypeExpr::Binary(_) => todo!(),
            TypeExpr::Unary(_) => todo!(),
        }
    }
}

impl ContextDisplay for TypeSymbol {
    fn display(&self, context: &Context) -> String {
        let name = context.lookup(context.database.type_names[self]);
        let TypeSymbol {
            symbol_id,
            module_id,
        } = self;
        format!("{name}{COMPILER_BRAND}{module_id}.{symbol_id}")
    }
}

impl ContextDisplay for TypeRefExpr {
    fn display(&self, context: &Context) -> String {
        self.symbol.display(context)
    }
}
