parse_primary = parser
  | [< 'Token.Number n >] -> Ast.Number n
