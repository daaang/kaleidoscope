type token =
  (* commands *)
  | Def | Extern

  (* primary *)
  | Ident of string | Number of float

  (* unknown *)
  | Kwd of char

let rec lex = parser
  (* skip whitespace *)
  | [< ' (' ' | '\n' | '\r' | '\t'); stream >] -> lex stream

  (* commands and idents *)
  | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_ident buffer stream

and lex_ident buffer = parser
  | [< ' ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_ident buffer stream
  | [< stream=lex >] ->
      match Buffer.contents buffer with
      | "def" -> [< 'Token.Def; stream >]
      | "extern" -> [< 'Token.Extern; stream >]
      | id -> [< 'Token.Ident id; stream >]
