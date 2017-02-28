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
