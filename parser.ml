parse_primary = parser
  | [< 'Token.Number n >] -> Ast.Number n
  | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e
