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
            TypeExpr::Union(union_type_expr) => {
                // TODO - would be nice to be able to show the user-given name in some cases
                // needs to *not* be shown as part of the type binding itself
                // i.e.
                // Color~1.0 := Color;
                //              ^^^^^ not useful, needs to be `red | green | blue`

                // if let Some(name) = union_type_expr.name {
                //     context.lookup(name).to_owned()
                // } else {
                let mut s = String::new();
                let len = union_type_expr.variants.len();
                for (i, variant) in union_type_expr.variants.iter().enumerate() {
                    s.push_str(context.lookup(variant.0));
                    s.push_str(": ");
                    s.push_str(&variant.1.display(context));
                    if i < len - 1 {
                        s.push_str(" | ");
                    }
                }
                s
                // }
            }
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
