(* OCaml declarations: *)
{
    open Lexing
    open Parser

    (* Create an exception we can use. *)
    exception Error of string

    (* Create a hash table that allows us to see if an identifier is a keyword *)
    let keywords : (string, Parser.token) Hashtbl.t = Hashtbl.create 64

    (* Make a list of all the keywords in the language. *)
    let keywords_list = [
        ("if", IF);
        ("then", THEN);
        ("else", ELSE);
        ("elsif", ELSIF);
        ("endif", ENDIF);
        ("repeat", REPEAT);
        ("until", UNTIL);
        ("for", FOR);
        ("to", TO);
        ("endfor", ENDFOR);
        ("case", CASE);
        ("of", OF);
        ("when", WHEN);
        ("otherwise", OTHERWISE);
        ("endcase", ENDCASE);
        ("return", RETURN);
        ("bit", BIT);
        ("bitstring", BITSTRING);
        ("integer", INTEGER);
        ("boolean", BOOLEAN);
        ("real", REAL);
        ("enumeration", ENUMERATION);
        ("list", LIST);
        ("array", ARRAY);
        ("constant", CONSTANT);
        ("assert", ASSERT);
        ("SEE", SEE);
        ("MOD", MOD);
        ("DIV", DIV);
        ("IN", IN);

        ("AND", AND);
        ("OR", OR);
        ("EOR", EOR);

        ("TRUE", TRUE);
        ("FALSE", FALSE);
        ("UNKNOWN", UNKNOWN);
        ("UNDEFINED", UNDEFINED);
        ("UNPREDICTABLE", UNPREDICTABLE);
        ("IMPLEMENTATION_DEFINED", IMPLEMENTATION_DEFINED);
        ("SUBARCHITECTURE_DEFINED", SUBARCHITECTURE_DEFINED)
    ]

    (* Fill the hash table with all the keywords *)
    let () =
        List.iter
            (fun (key, token) -> Hashtbl.add keywords key token)
            keywords_list

    let get_keyword_or_identifier id =
        try Hashtbl.find keywords id
        with Not_found -> IDENTIFIER id

    let convert_string_to_integer string =
        let value = match String.get string 0 with
            | '\'' | '\"' -> "0b" ^ (String.sub string 1 (String.length string - 2))
            | _ -> string
        in
            int_of_string value

    let remove_quotes string =
        let len = String.length string in
        String.sub string 1 (len - 2)
}

(* Lexer definitions: *)
(* Quotations *)
let single_quote = '\''
let double_quote = '"'

(* Non digit *)
let nondigit = ['_' 'a'-'z' 'A'-'Z']

(* Digits *)
let base2_digit = ['0' '1']
let base10_digit = ['0'-'9']
let base16_digit = ['0'-'9' 'A'-'F' 'a'-'f']

(* Identifier *)
let identifier = nondigit (nondigit | base10_digit)*

(* Whitespaces *)
let whitespace_char_no_newline = [' ' '\t']

(* New lines *)
let newline = "\n" | "\r" | "\r\n"

(* Binary numbers *)
let base2_constant =
      single_quote base2_digit+ single_quote
    | double_quote base2_digit+ double_quote

(* Decimal numbers *)
let base10_constant = base10_digit+

(* Hexadecimal numbers *)
let base16_prefix = "0x" | "0X"
let base16_constant = base16_prefix base16_digit+

(* Integers *)
let integer_constant =
      base2_constant
    | base10_constant
    | base16_constant

(* Masked binary numbers *)
let base2_constant_masked = ['0' '1' 'x']
let integer_constant_masked =
      single_quote base2_constant_masked+ single_quote
    | double_quote base2_constant_masked+ double_quote

(* Regular expressions and actions: *)
(* This function is accessible from Lexer.tokenize from our .ml file. *)
rule tokenize = parse
    | whitespace_char_no_newline+   { tokenize lexbuf }
    | newline                       { initial_linebegin lexbuf ; EOL }
    | "/*"                          { multiline_comment lexbuf; tokenize lexbuf }
    | "//"                          { singleline_comment lexbuf; tokenize lexbuf }
    | integer_constant              { INTEGER_CONSTANT (convert_string_to_integer (Lexing.lexeme lexbuf)) }
    | integer_constant_masked       { INTEGER_CONSTANT_MASKED (remove_quotes (Lexing.lexeme lexbuf)) }
    | "<<"                          { LTLT }
    | ">>"                          { GTGT }
    | "=="                          { EQEQ }
    | "!="                          { NEQ }
    | "<="                          { LEQ }
    | ">="                          { GEQ }
    | "="                           { EQ }
    | "<"                           { LT }
    | ">"                           { GT }
    | "+"                           { PLUS }
    | "-"                           { MINUS }
    | "*"                           { STAR }
    | "/"                           { SLASH }
    | "!"                           { BANG }
    | "&&"                          { AMPAMP }
    | "||"                          { BARBAR }
    | "^"                           { HAT }
    | ":"                           { COLON }
    | "~"                           { TILDE }
    | "{"                           { LBRACE }
    | "}"                           { RBRACE }
    | "["                           { LBRACK }
    | "]"                           { RBRACK }
    | "("                           { LPAREN }
    | ")"                           { RPAREN }
    | ";"                           { SEMICOLON }
    | ","                           { COMMA }
    | "."                           { DOT }
    | identifier                    { get_keyword_or_identifier (Lexing.lexeme lexbuf) }
    | eof                           { EOF }
    | _                             { failwith "Lexer error" }

and initial_linebegin = parse
    | newline                       { Lexing.new_line lexbuf; initial_linebegin lexbuf }
    | whitespace_char_no_newline    { initial_linebegin lexbuf }
    | ""                            { tokenize lexbuf }

and multiline_comment = parse
    | "*/"                          { () }
    | eof                           { failwith "Unterminated comment" }
    | newline                       { Lexing.new_line lexbuf; multiline_comment lexbuf }
    | _                             { multiline_comment lexbuf }

and singleline_comment = parse
    | newline                       { Lexing.new_line lexbuf }
    | eof                           { () }
    | _                             { singleline_comment lexbuf }

