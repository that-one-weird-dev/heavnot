let parse_type (tokens : Token.t list) =
  let open Type in
  match tokens with
  | Int :: tokens -> (tokens, Int)
  | Float :: tokens -> (tokens, Float)
  | String :: tokens -> (tokens, String)
  | Identifier id :: tokens -> (tokens, Reference id)
  | token :: _ -> raise (Error.invalid_token token)
  | [] -> raise Error.unexpected_eof
