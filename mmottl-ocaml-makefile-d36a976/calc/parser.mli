type token =
  | INT of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
