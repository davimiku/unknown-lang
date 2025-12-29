use expect_test::expect;

use crate::grammar::check_expr;

#[test]
fn parse_record_two_fields() {
    check_expr("[ field_a = value_a, field_b = value_b ]", expect![[r#""#]])
}
