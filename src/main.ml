(* ocamlbuild -tag thread -use-ocamlfind -pkgs 'core,core_extended' main.native *)
open List
open String
open Core_extended

(* Open the lexer we've created *)
open Lexer
open Lexing
open Ast

let parse_stdin () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      ignore (Parser.expression Lexer.tokenize lexbuf);
      Printf.printf "Parsed ok\n";
      flush stdout
    done
  with e ->
    Printf.printf "Error!\n";
    exit 0

let parse_string code =
  let buffer = Lexing.from_string code in
  Parser.expression Lexer.tokenize buffer

let _ =
  let tree = parse_string "2 * 4 - 8 / 16 * 32 * 128\n" in
  print_tree tree [Root];

  Printf.printf "Testing parsing of expressions:\n";
  Printf.printf "Expression -> '%s'\n" (parse_string "1+2+3+4\n" |> expression_to_string)
