use la_arena::Idx;

use crate::{Context, ContextDisplay, COMPILER_BRAND};

use super::{TypeExpr, TypeRefExpr, TypeSymbol, TypeVarDefExpr};

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

            TypeExpr::FloatLiteral(f) => f.to_string(),
            TypeExpr::IntLiteral(i) => i.to_string(),
            TypeExpr::StringLiteral(key) => format!(r#""{}""#, context.lookup(*key)),

            TypeExpr::Unit => "()".to_string(),
            TypeExpr::VarRef(type_ref) => type_ref.display(context),
            TypeExpr::UnresolvedVarRef { .. } => todo!(),

            TypeExpr::VarDef(type_var_def) => {
                let mut s = String::new();
                let TypeVarDefExpr { symbol, type_expr } = type_var_def;
                s.push_str(&symbol.display(context).to_string());
                s.push_str(" := ");
                s.push_str(&type_expr.display(context).to_string());
                s
            }
            TypeExpr::Union(_) => todo!(),
            TypeExpr::Call(_) => todo!(),
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
