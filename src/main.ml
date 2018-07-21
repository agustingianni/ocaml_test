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
  Parser.program Lexer.tokenize buffer

let _ =
  try
    (* Printf.printf "Multiplicative expressions:\n";
    print_expression_tree "a * b";
    print_expression_tree "a / b";
    print_expression_tree "a DIV b";
    print_expression_tree "a MOD b";
    print_expression_tree "a : b";

    Printf.printf "Additive expressions:\n";
    print_expression_tree "a + b";
    print_expression_tree "a - b";

    Printf.printf "Shift expressions:\n";
    print_expression_tree "a << b";
    print_expression_tree "a >> b";

    Printf.printf "Relational expressions:\n";
    print_expression_tree "a < b";
    print_expression_tree "a > b";
    print_expression_tree "a <= b";
    print_expression_tree "a >= b";
    print_expression_tree "a IN b";

    Printf.printf "Equality expressions:\n";
    print_expression_tree "a == b";
    print_expression_tree "a != b";

    Printf.printf "Bitwise expressions:\n";
    print_expression_tree "a AND b";
    print_expression_tree "a OR b";
    print_expression_tree "a EOR b";

    Printf.printf "Logical expressions:\n";
    print_expression_tree "a && b";
    print_expression_tree "a || b";
    print_expression_tree "a ^ b";

    Printf.printf "Assignment expressions:\n";
    print_expression_tree "a = b";

    Printf.printf "Unary expressions:\n";
    print_expression_tree "+a";
    print_expression_tree "-a";
    print_expression_tree "!a";
    print_expression_tree "~a";

    print_expression "(1+2+3+4+5)";
    print_expression "aaaa[1]"; *)

    ignore (print_program "real fname() {}");
    ignore (print_program "real variable;");
    ignore (print_program "real variable = 1000;");

    (* let token = parse_string "1\n" in
    Printf.printf "Is equal = %b\n" (token = (ConstantExpression (IntegerValue 2))); *)

  with e ->
    Printf.printf "Exception -> %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
