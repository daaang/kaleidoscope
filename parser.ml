parse_primary = parser
  | [< 'Token.Number n >] -> Ast.Number n

  | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e

  | [< 'Token.Ident id; stream >] ->
      let rec parse_args accumulator = parser
        | [< e=parse_expr; stream >] ->
            begin parser
              | [< 'Token.Kwd ','; e=parse_args (e :: accumulator) >] -> e
              | [< >] -> e :: accumulator
            end stream
        | [< >] -> accumulator
      in
      let rec parse_ident id = parser
        | [< 'Token.Kwd '(';
              args=parse_args [];
              'Token.Kwd ')' ?? "expected ')'" >] ->
            Ast.Call (id, Array.of_list (List.rev args))

        | [< >] -> Ast.Variable id
      in
      parse_ident id stream

  | [< >] -> raise (Stream.Error "unknown token when expecting an expression.")
