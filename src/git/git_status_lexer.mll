{
  let status = Git_status_code.of_char
}

let sp     = ' '
let status = [' ' 'M' 'A' 'D' 'R' 'C' 'U' '?']
let arrow  = "->"
let lf     = '\n'

rule parse_status = parse
  | eof
    {`Eof}

  | (status as x) (status as y) sp ([^ '\n']+ as p) lf
    {`Status (Git_status.Normal (status x, status y, p))}

  | (status as x) (status as y) sp ([^ '\n']+ as p1) sp arrow sp (_* as p2) lf
    {`Status (Git_status.Rename (status x, status y, p1, p2))}

  | ([^ '\n']+ as other) lf
    {
      (* TODO: Consider returning Result.t *)
      Printf.eprintf "Unexpected status line: %S\n" other;
      parse_status lexbuf
    }
