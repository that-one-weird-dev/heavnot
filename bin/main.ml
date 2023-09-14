open Heavnot

let read_source_file path =
  let in_channel = open_in path in
  let rec read_lines acc =
    try
      let line = input_line in_channel in
      read_lines (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let lines = read_lines "" in
  close_in in_channel;
  lines

let () =
  let lines = read_source_file "heavnot-src/test1.hvn" in
  let tokens = Lexer.tokenize lines in
  let root = Parser.parse tokens in

  let scope = Tcheck.Scope.create None in
  Tcheck.Scope.set scope "print"
    (Type.Function { params = [ Type.String ]; return = Type.Unit });

  Tcheck.check_root scope root;

  let scope = Interpreter.Scope.create None in

  let print =
    Interpreter.Value.external_function (fun params ->
        (match params with
        | param :: _ -> print_endline (Interpreter.Value.show param)
        | [] -> ());
        Interpreter.Value.unit ())
  in
  Interpreter.Scope.set scope "print" print;

  Interpreter.execute_root scope root;
  let result = Interpreter.execute_function scope "main" in

  print_endline (Interpreter.Value.show result)
