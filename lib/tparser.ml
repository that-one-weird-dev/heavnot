let rec parse_type (tokens : Token.t list) =
  let open Type in
  match tokens with
  | Unit :: tokens -> (tokens, Unit)
  | Int :: tokens -> (tokens, Int)
  | Float :: tokens -> (tokens, Float)
  | String :: tokens -> (tokens, String)
  | Identifier id :: tokens -> (tokens, Reference id)
  | BraceOpen :: tokens -> parse_object_type [] tokens
  | ParenOpen :: tokens -> parse_function_type [] tokens
  | token :: _ -> raise (Error.invalid_token token)
  | [] -> raise Error.unexpected_eof

and parse_object_type (fields : (string * Type.t) list) (tokens : Token.t list)
    =
  match tokens with
  | BraceClose :: tokens -> (tokens, Type.Object fields)
  | Identifier id :: Colon :: tokens -> (
      let tokens, type_ = parse_type tokens in
      let field = (id, type_) in

      match tokens with
      | BraceClose :: tokens -> (tokens, Type.Object (field :: fields))
      | Comma :: tokens -> parse_object_type (field :: fields) tokens
      | token :: _ -> raise (Error.invalid_token token)
      | [] -> raise Error.unexpected_eof)
  | token :: _ -> raise (Error.invalid_token token)
  | [] -> raise Error.unexpected_eof

and parse_function_type (params : Type.t list) (tokens : Token.t list) =
  let result tokens params =
    let tokens, return = parse_type tokens in
    (tokens, Type.Function { params; return })
  in

  match tokens with
  | ParenClose :: Colon :: tokens -> result tokens params
  | tokens -> (
      let tokens, param = parse_type tokens in

      match tokens with
      | ParenClose :: Colon :: tokens -> result tokens (param :: params)
      | Comma :: tokens -> parse_function_type (param :: params) tokens
      | token :: _ -> raise (Error.unexpected_token Token.BraceClose token)
      | [] -> raise Error.unexpected_eof)
