let unexpected_token expected actual =
  Failure
    ("Invalid token " ^ Token.show actual ^ " expected " ^ Token.show expected)

let invalid_token token = Failure ("Invalid token " ^ Token.show token)
let unexpected_eof = Failure "Unexpected eof"
