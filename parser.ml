let binop_precedence:(char, int) Hashtbl.t = Hashtbl.create 10
let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

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

and parse_expr = parser
  | [< lhs=parse_primary; stream >] -> parse_bin_rhs 0 lhs stream
