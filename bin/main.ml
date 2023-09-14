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

  Tcheck.check root;

  let scope = Interpreter.execute root in
  let result = Interpreter.execute_function scope "main" in

  print_endline (Interpreter.Value.show result)
