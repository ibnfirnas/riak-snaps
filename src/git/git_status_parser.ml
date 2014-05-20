let parse git_status_output =
  let lexbuf = Lexing.from_string git_status_output in
  let rec parse statuses =
    match Git_status_lexer.parse_status lexbuf with
    | `Status s -> parse (s :: statuses)
    | `Eof      -> statuses
  in
  parse []
