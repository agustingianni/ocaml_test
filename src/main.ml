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
      ignore (Parser.test_expression Lexer.tokenize lexbuf);
      Printf.printf "Parsed ok\n";
      flush stdout
    done
  with e ->
    Printf.printf "Error!\n";
    exit 0

let parse_string code =
  let buffer = Lexing.from_string code in
  Parser.test_expression Lexer.tokenize buffer

let print_expression expression =
  parse_string expression |> pp_expression |> Printf.printf "expression -> %s\n"

let print_expression_tree expression =
  parse_string expression |> print_tree

let print_program program =
  let buffer = Lexing.from_string program in
  pp_program (Parser.program Lexer.tokenize buffer)

let test_variable_declarations () =
  print_program "integer variable;";
  print_program "boolean variable;";
  print_program "real variable;";
  print_program "bit variable;";
  print_program "bits(32) variable;";

  print_program "integer variable = 1;";
  print_program "boolean variable = TRUE;";
  print_program "real variable = 1.0;";
  print_program "bit variable = '1';";
  print_program "bits(32) variable = '1';";
  print_program "array bits(64) RC[32];"

let test_function_declaration () =
  print_program "real fname() {};"

let test_program () =
  print_program
  {|

    real global_variable;

    real fname() {
      integer var00;
      1+2;
      fname();
    }

  |}

let _ =
  try
    test_program ();

    (* test_variable_declarations (); *)
    (* test_function_declaration (); *)

    (* print_program "enumeration enum_name { SomeID };"; *)
    (* print_program "(integer hola) pepe;"; *)

  with e ->
    Printf.printf "Exception -> %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
