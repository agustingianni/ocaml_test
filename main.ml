(* ocamlbuild -tag thread -use-ocamlfind -pkgs 'core,core_extended' main.native *)
open List
open String
open Core_extended

(* Give a string return a dictionary of the words and the amount of times they are used *)
let add_word_count table word =
  let count = match Hashtbl.find_opt table word with
    | None -> 0
    | Some x -> x + 1
  in
  Hashtbl.replace table word count

let get_words_from_string ?(sep=' ') string =
  String.split_on_char sep string

let get_word_frequency string =
  let words = get_words_from_string string in
  let freq = Hashtbl.create 4096 in
  List.iter (fun word -> add_word_count freq word) words;
  freq

let print_word word frequency =
  Printf.printf "word = %s frequency = %d\n" word frequency

type host_info = {
  host_name : string;
  os_name   : string;
  cpu_arch  : string;
}

let my_computer = {
  host_name = Shell.sh_one_line_exn "hostname";
  os_name = Shell.sh_one_line_exn "uname -s";
  cpu_arch = Shell.sh_one_line_exn "uname -p";
}

(* Open the lexer we've created *)
open Lexer
open Lexing
open Ast

(* Unit tests *)
open OUnit2

let parse_string code =
  let buffer = Lexing.from_string code in
  Parser.expression Lexer.tokenize buffer

let make_integer_expression value =
  ConstantExpression (IntegerValue value)

let make_boolean_expression value =
  ConstantExpression (BooleanValue value)

let make_variable_expression value =
  VariableExpression (Identifier value)

(* Tests for IntegerValue *)
let integer_value_tests = "integer_value_tests" >::: [
    (* Test base2 with single quotes *)
    "test_00" >:: (fun ctx -> assert_equal (parse_string "'0'") (make_integer_expression 0));
    "test_01" >:: (fun ctx -> assert_equal (parse_string "'00'") (make_integer_expression 0));
    "test_02" >:: (fun ctx -> assert_equal (parse_string "'000'") (make_integer_expression 0));
    "test_03" >:: (fun ctx -> assert_equal (parse_string "'0000'") (make_integer_expression 0));
    "test_04" >:: (fun ctx -> assert_equal (parse_string "'1'") (make_integer_expression 1));
    "test_05" >:: (fun ctx -> assert_equal (parse_string "'10'") (make_integer_expression 2));
    "test_06" >:: (fun ctx -> assert_equal (parse_string "'11'") (make_integer_expression 3));
    "test_07" >:: (fun ctx -> assert_equal (parse_string "'100'") (make_integer_expression 4));

    (* Test base2 with double quotes *)
    "test_08" >:: (fun ctx -> assert_equal (parse_string "\"0\"") (make_integer_expression 0));
    "test_09" >:: (fun ctx -> assert_equal (parse_string "\"1\"") (make_integer_expression 1));
    "test_10" >:: (fun ctx -> assert_equal (parse_string "\"10\"") (make_integer_expression 2));
    "test_11" >:: (fun ctx -> assert_equal (parse_string "\"11\"") (make_integer_expression 3));
    "test_12" >:: (fun ctx -> assert_equal (parse_string "\"100\"") (make_integer_expression 4));

    (* Test base10 *)
    "test_13" >:: (fun ctx -> assert_equal (parse_string "0") (make_integer_expression 0));
    "test_14" >:: (fun ctx -> assert_equal (parse_string "00") (make_integer_expression 0));
    "test_15" >:: (fun ctx -> assert_equal (parse_string "1") (make_integer_expression 1));
    "test_16" >:: (fun ctx -> assert_equal (parse_string "10") (make_integer_expression 10));
    "test_17" >:: (fun ctx -> assert_equal (parse_string "100") (make_integer_expression 100));

    (* Test base16 *)
    "test_18" >:: (fun ctx -> assert_equal (parse_string "0x0") (make_integer_expression 0));
    "test_19" >:: (fun ctx -> assert_equal (parse_string "0X0") (make_integer_expression 0));
    "test_20" >:: (fun ctx -> assert_equal (parse_string "0x00") (make_integer_expression 0));
    "test_21" >:: (fun ctx -> assert_equal (parse_string "0X00") (make_integer_expression 0));
    "test_22" >:: (fun ctx -> assert_equal (parse_string "0xcafe") (make_integer_expression 0xcafe));
    "test_23" >:: (fun ctx -> assert_equal (parse_string "0Xcafe") (make_integer_expression 0xcafe));
]

let boolean_value_tests = "boolean_value_tests" >::: [
    "test_00" >:: (fun ctx -> assert_equal (parse_string "FALSE") (make_boolean_expression false));
    "test_01" >:: (fun ctx -> assert_equal (parse_string "TRUE") (make_boolean_expression true));
]

let identifier_tests = "identifier_tests" >::: [
    "test_00" >:: (fun ctx -> assert_equal (parse_string "example0") (make_variable_expression "example0"));
    (* "test_01" >:: (fun ctx -> assert_equal (parse_string "0example") (make_variable_expression "example")); *)
]

let variable_declarations = "variable_declarations" >::: [
]

let parse_as_string string =
  parse_string string |> expression_to_string

let _ =
  (* run_test_tt_main integer_value_tests;
  run_test_tt_main boolean_value_tests;
  run_test_tt_main identifier_tests; *)

  Printf.printf "DEBUG: %s\n" (parse_as_string "1");
  Printf.printf "DEBUG: %s\n" (parse_as_string "TRUE");
  Printf.printf "DEBUG: %s\n" (parse_as_string "FALSE");
  Printf.printf "DEBUG: %s\n" (parse_as_string "variable");
    
  (* ignore (parse_string "bit single_bit;");
  ignore (parse_string "bits(1) single_bit;");
  ignore (parse_string "bits(2) double_bit;");
  ignore (parse_string "bits(N) n_bit;");
  ignore (parse_string "integer integer_var;");
  ignore (parse_string "real real_var;");
  ignore (parse_string "boolean boolean_var;");
  ignore (parse_string "enumeration enum_name {e0, e1, e2, e3};"); *)

  (* print_endline "Parsing code ...";

  let code = "'11'" in
  let buffer = Lexing.from_string code in
  try
    let s = node_to_string (Parser.program Lexer.tokenize buffer) in
    print_endline s

  with e ->
    let msg = Printexc.to_string e in
    let stack = Printexc.get_backtrace () in
    Printf.eprintf "Error: %s%s\n" msg stack;
    raise e *)