type expr =
  (* variant for numeric literals, e.g. "1.0" *)
  | Number of float

  (* variant for referencing a variable, e.g. "a" *)
  | Variable of string

  (* variant for a binary operator, e.g. "+" *)
  | Binary of char * expr * expr

  (* variant for function calls *)
  | Call of string * expr array
