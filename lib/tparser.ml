let rec parse_type (tokens : Token.t list) =
  let open Type in
  match tokens with
  | Int :: tokens -> (tokens, Int)
  | Float :: tokens -> (tokens, Float)
  | String :: tokens -> (tokens, String)
  | Identifier id :: tokens -> (tokens, Reference id)
  | BraceOpen :: tokens -> parse_object_type [] tokens
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
      | Comma :: tokens -> parse_object_type (field :: fields) tokens
      | BraceClose :: tokens -> (tokens, Type.Object (field :: fields))
      | token :: _ -> raise (Error.invalid_token token)
      | [] -> raise Error.unexpected_eof)
  | token :: _ -> raise (Error.invalid_token token)
  | [] -> raise Error.unexpected_eof
