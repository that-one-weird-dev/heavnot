open Error

let rec parse_parameters params (tokens : Token.t list) =
  match tokens with
  | ParenClose :: tokens -> (tokens, List.rev params)
  | Identifier id :: Colon :: tokens -> (
      let tokens, type_ = Tparser.parse_type tokens in
      let param : Ast.param = { identifier = id; type_ } in

      match tokens with
      | Token.Comma :: tokens -> parse_parameters (param :: params) tokens
      | ParenClose :: tokens -> (tokens, List.rev (param :: params))
      | token :: _ -> raise (invalid_token token)
      | [] -> raise unexpected_eof)
  | token :: _ -> raise (invalid_token token)
  | [] -> raise unexpected_eof

and parse_parameter_statements statements (tokens : Token.t list) =
  match tokens with
  | ParenClose :: tokens -> (tokens, List.rev statements)
  | _ :: _ -> (
      let tokens, statement = parse_statement tokens in
      match tokens with
      | Token.Comma :: tokens ->
          parse_parameter_statements (statement :: statements) tokens
      | ParenClose :: tokens -> (tokens, List.rev (statement :: statements))
      | token :: _ -> raise (invalid_token token)
      | [] -> raise unexpected_eof)
  | [] -> raise unexpected_eof

and parse_body body (tokens : Token.t list) =
  match tokens with
  | BraceClose :: tokens -> (tokens, List.rev body)
  | Semicolon :: tokens -> parse_body body tokens
  | _ :: _ ->
      let tokens, statement = parse_statement tokens in
      parse_body (statement :: body) tokens
  | [] -> raise unexpected_eof

and parse_variable (tokens : Token.t list) id (type_ : Type.t option) =
  let open Ast in
  let tokens, value = parse_statement tokens in
  (tokens, VariableDecl { identifier = id; type_; value })

and parse_if (tokens : Token.t list) =
  let tokens, condition = parse_statement tokens in

  let tokens, then_body =
    match tokens with
    | BraceOpen :: tokens -> parse_body [] tokens
    | token :: _ -> raise (unexpected_token Token.BraceOpen token)
    | [] -> raise unexpected_eof
  in

  let tokens, else_body =
    match tokens with
    | Else :: BraceOpen :: tokens -> parse_body [] tokens
    | tokens -> (tokens, [])
  in

  (tokens, Ast.IfExpression { condition; then_body; else_body })

and parse_match_body branches (tokens : Token.t list) =
  match tokens with
  | Underscore :: Equal :: Greater :: BraceOpen :: tokens ->
      let tokens, body = parse_body [] tokens in
      let branch : Ast.match_branch = { variant = Ast.MatchDefault; body } in
      parse_match_body (branch :: branches) tokens
  | Identifier variant
    :: Identifier var_identifier
    :: Equal :: Greater :: BraceOpen :: tokens ->
      let tokens, body = parse_body [] tokens in
      let variant =
        Ast.MatchIdentifier { identifier = variant; var_identifier }
      in
      let branch : Ast.match_branch = { variant; body } in
      parse_match_body (branch :: branches) tokens
  | BraceClose :: tokens -> (tokens, List.rev branches)
  | token :: _ -> raise (invalid_token token)
  | [] -> raise unexpected_eof

and parse_match (tokens : Token.t list) =
  let open Ast in
  let tokens, value = parse_statement tokens in

  let tokens =
    match tokens with
    | BraceOpen :: tokens -> tokens
    | token :: _ -> raise (unexpected_token Token.BraceOpen token)
    | [] -> raise unexpected_eof
  in

  let tokens, branches = parse_match_body [] tokens in
  (tokens, MatchExpression { value; branches })

and parse_identifier (tokens : Token.t list) id =
  match tokens with
  | Is :: tokens ->
      let tokens, type_ = Tparser.parse_type tokens in
      (tokens, Ast.TypeDecl { identifier = id; type_ })
  | Colon :: Colon :: Identifier identifier :: tokens ->
      (tokens, TypeAccess { type_ = Type.Reference id; identifier })
  | Colon :: Equal :: tokens -> parse_variable tokens id None
  | Colon :: tokens ->
      let tokens, type_ = Tparser.parse_type tokens in
      let tokens =
        match tokens with
        | Equal :: tokens -> tokens
        | token :: _ -> raise (unexpected_token Token.Equal token)
        | [] -> raise unexpected_eof
      in
      parse_variable tokens id (Some type_)
  | _ :: _ -> (tokens, VariableAccess id)
  | [] -> raise unexpected_eof

and parse_suffix (tokens : Token.t list) statement =
  let open Ast in
  match tokens with
  | ParenOpen :: tokens ->
      let tokens, statements = parse_parameter_statements [] tokens in

      parse_suffix tokens
        (FunctionCall { value = statement; params = statements })
  | Dot :: Identifier identifier :: tokens ->
      parse_suffix tokens (ObjectAccess { value = statement; identifier })
  | Semicolon :: _ -> (tokens, statement)
  | tokens -> (tokens, statement)

and parse_statement (tokens : Token.t list) =
  let tokens, statement =
    match tokens with
    | Identifier id :: tokens -> parse_identifier tokens id
    | ParenOpen :: ParenClose :: tokens -> (tokens, UnitLiteral)
    | IntLiteral value :: tokens -> (tokens, IntLiteral value)
    | FloatLiteral value :: tokens -> (tokens, FloatLiteral value)
    | StringLiteral value :: tokens -> (tokens, StringLiteral value)
    | BoolLiteral value :: tokens -> (tokens, BoolLiteral value)
    | Fn :: Identifier identifier :: ParenOpen :: tokens -> parse_function identifier tokens
    | ParenOpen :: tokens -> (
        let tokens, statement = parse_statement tokens in
        match tokens with
        | ParenClose :: tokens -> (tokens, statement)
        | token :: _ -> raise (unexpected_token Token.ParenClose token)
        | [] -> raise unexpected_eof)
    | BraceOpen :: tokens -> parse_object [] tokens
    | If :: tokens -> parse_if tokens
    | Match :: tokens -> parse_match tokens
    | token :: _ -> raise (invalid_token token)
    | [] -> raise unexpected_eof
  in

  parse_suffix tokens statement

and parse_function identifier (tokens : Token.t list) =
  let tokens, params = parse_parameters [] tokens in

  let tokens, return_type =
    match tokens with
    | Colon :: tokens -> Tparser.parse_type tokens
    | _ -> (tokens, Type.Unit)
  in

  let tokens =
    match tokens with
    | BraceOpen :: tokens -> tokens
    | token :: _ -> raise (invalid_token token)
    | [] -> raise unexpected_eof
  in

  let tokens, body = parse_body [] tokens in
  (tokens, FunctionDecl { identifier : string; params; return_type; body })

and parse_object values (tokens : Token.t list) =
  match tokens with
  | Identifier id :: Colon :: tokens -> (
      let tokens, statement = parse_statement tokens in
      let value = (id, statement) in

      match tokens with
      | Comma :: tokens -> parse_object (value :: values) tokens
      | BraceClose :: tokens -> (tokens, ObjectLiteral (value :: values))
      | token :: _ -> raise (unexpected_token Token.BraceClose token)
      | [] -> raise unexpected_eof)
  | BraceClose :: tokens -> (tokens, ObjectLiteral values)
  | token :: _ -> raise (unexpected_token Token.BraceClose token)
  | [] -> raise unexpected_eof

let rec parse_root_body body (tokens : Token.t list) =
  match tokens with
  | _ :: _ ->
      let tokens, statement = parse_statement tokens in
      parse_root_body (statement :: body) tokens
  | [] -> List.rev body

let parse tokens : Ast.root =
  let body = parse_root_body [] tokens in
  { body }
