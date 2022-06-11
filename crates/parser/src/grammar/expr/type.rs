use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;

// TODO
// What kinds of type expressions are possible? Binary expressions?
//
// JS calls `.` "member access" and `[...]` "computed member access"
// (that's a pretty good description actually)
// Both are left-to-right binary operations with almost the highest precedence
// (highest except for parentheses grouping)
//
// exotic examples:
// 1. "indexed access types" from TS: `config.Options["port"]`
// concerns: could conflict with square bracket generics
// possibility: `config.Options.port`
// any reason why not? why a struct field couldn't be accessed by the same path operator?
//
// 2. Conditional types
// for some generic T
// `if T == true { string } else { int }`
//
// 3. Key of (right-to-left unary)
// `keyof MyStruct`
pub(super) fn type_expr_binding_power(
    p: &mut Parser,
    minimum_binding_power: u8,
) -> Option<CompletedMarker> {
    // let mut lhs = parse_lhs(p)?;

    loop {
        break;
        // let op = if p.at(TokenKind::Plus) {
        //     BinaryOp::Add
        // } else if p.at(TokenKind::Dash) {
        //     BinaryOp::Sub
        // } else if p.at(TokenKind::Star) {
        //     BinaryOp::Mul
        // } else if p.at(TokenKind::Slash) {
        //     BinaryOp::Div
        // } else if p.at(TokenKind::Percent) {
        //     BinaryOp::Rem
        // } else if p.at(TokenKind::Caret) {
        //     BinaryOp::Exp
        // } else if p.at(TokenKind::And) {
        //     BinaryOp::And
        // } else if p.at(TokenKind::Or) {
        //     BinaryOp::Or
        // } else {
        //     // Not at an operator, let the caller decide what to do next
        //     break;
        // };

        // let (left_binding_power, right_binding_power) = op.binding_power();

        // if left_binding_power < minimum_binding_power {
        //     break;
        // }

        // Consume the operator token
        // p.bump();

        // let m = lhs.precede(p);
        // let parsed_rhs = expr_binding_power(p, right_binding_power).is_some();
        // lhs = m.complete(p, SyntaxKind::InfixExpr);

        // if !parsed_rhs {
        //     break;
        // }
    }

    Some(todo!())
}
