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
  print_program "array bits(64) RC[32];";
  print_program "integer v0, v1;"

let test_function_declaration () =
  print_program "real fname() {};"

let test_type_declaration () =
  print_program "type ShiftSpec is (bits(2) shift, integer amount)"

let test_enumeration_declaration () =
  print_program "enumeration ExampleEnumeration {val1, val2, val3}"

let test_if_statement () =
  print_program
  {|
    integer realTest() {
      if n == 1 then
        CheckSPAlignment();
        address = SP;
      endif
    }
  |}

let test_program () =
  print_program
  {|

    type AddressDescriptor is (
      FaultRecord fault,
      FullAddress paddress,
      bits(64) vaddress
    )

    real global_variable;
    array real global_array[128];

    enumeration TestEnum { val }

    type TestStruct is ( integer val )

    real fname(boolean arg0, integer arg1, bit arg2, bits(32) arg3, TestEnum arg4, TestStruct arg5) {
      integer var00 = 1000;
      1+2;
      fname();

      enumeration TestEnum { val }
    }

    (integer, integer) returnList(boolean yes) {
    }

    integer realTest() {
      CheckFPAdvSIMDEnabled64();
      bits(64) address;
      bits(64) offs;
      bits(datasize) rval;
      constant integer ebytes = esize DIV 8;
      integer e, r, s, tt;

      if n == 31 then
          address = 1;
          address = 2;
          address = 3;
          address = 4;
      endif
    }

  |}

let _ =
  try
    test_program ();

    (* test_type_declaration (); *)
    (* test_enumeration_declaration (); *)

    (* test_variable_declarations (); *)
    (* test_function_declaration (); *)

    (* print_program "enumeration enum_name { SomeID };"; *)
    (* print_program "(integer hola) pepe;"; *)

  with e ->
    Printf.printf "Exception -> %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stdout;
