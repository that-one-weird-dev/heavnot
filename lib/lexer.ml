type number_type = IntNumber of int | FloatNumber of float

let invalid_character pos =
  raise (Failure ("Invalid character found at " ^ string_of_int pos))

let rec consume_word word input pos =
  if pos < String.length input then
    let char = input.[pos] in
    match char with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        consume_word (word ^ String.make 1 char) input (pos + 1)
    | _ -> (pos, word)
  else (pos, word)

let consume_word = consume_word ""

let rec consume_string str input pos =
  if pos < String.length input then
    let char = input.[pos] in
    match char with
    | '"' -> (pos + 1, str)
    | char -> consume_string (str ^ String.make 1 char) input (pos + 1)
  else (pos, str)

let consume_string = consume_string ""

let rec consume_number number is_float input pos =
  let parse_result () =
    if is_float then FloatNumber (float_of_string number)
    else IntNumber (int_of_string number)
  in

  if pos < String.length input then
    let char = input.[pos] in
    match char with
    | '0' .. '9' ->
        consume_number (number ^ String.make 1 char) is_float input (pos + 1)
    | '.' ->
        if is_float then invalid_character pos
        else consume_number (number ^ ".") true input (pos + 1)
    | _ -> (pos, parse_result ())
  else (pos, parse_result ())

let consume_number = consume_number "" false

let tokenize_statement input pos =
  let open Token in
  let char = input.[pos] in

  match char with
  | 'a' .. 'z' | 'A' .. 'Z' ->
      let pos, word = consume_word input pos in
      (pos, Some (Token.from_identifier word))
  | '0' .. '9' ->
      let pos, number = consume_number input pos in
      let token =
        match number with
        | IntNumber value -> IntLiteral value
        | FloatNumber value -> FloatLiteral value
      in
      (pos, Some token)
  | '"' ->
      let pos, str = consume_string input (pos + 1) in
      (pos, Some (StringLiteral str))
  | ' ' | '\n' -> (pos + 1, None)
  | '(' -> (pos + 1, Some ParenOpen)
  | ')' -> (pos + 1, Some ParenClose)
  | '[' -> (pos + 1, Some BracketOpen)
  | ']' -> (pos + 1, Some BracketClose)
  | '{' -> (pos + 1, Some BraceOpen)
  | '}' -> (pos + 1, Some BraceClose)
  | '=' -> (pos + 1, Some Equal)
  | ',' -> (pos + 1, Some Comma)
  | ':' -> (pos + 1, Some Colon)
  | '.' -> (pos + 1, Some Dot)
  | ';' -> (pos + 1, Some Semicolon)
  | '|' -> (pos + 1, Some Pipe)
  | '<' -> (pos + 1, Some Lesser)
  | '>' -> (pos + 1, Some Greater)
  | '_' -> (pos + 1, Some Underscore)
  | _ -> invalid_character pos

let rec tokenize tokens pos input =
  if pos < String.length input then
    let pos, token = tokenize_statement input pos in
    match token with
    | Some token -> tokenize (token :: tokens) pos input
    | None -> tokenize tokens pos input
  else List.rev tokens

let tokenize = tokenize [] 0
