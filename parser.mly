(* Import needed types from other files. *)
%{
    open Ast
%}

%token <int> INTEGER_CONSTANT
%token <string> IDENTIFIER
%token <string> TYPENAME
%token <string> INTEGER_CONSTANT_MASKED

(* Token definition for the languages keywords. *)
%token IF THEN ELSE ELSIF ENDIF
%token REPEAT UNTIL
%token FOR TO ENDFOR
%token CASE OF WHEN OTHERWISE ENDCASE
%token RETURN
%token TRUE FALSE
%token MOD DIV IN AND OR EOR LTLT GTGT EQEQ NEQ LEQ GEQ EQ LT GT PLUS MINUS STAR SLASH BANG AMPAMP BARBAR HAT COLON TILDE
%token LBRACE RBRACE LBRACK RBRACK LPAREN RPAREN
%token SEMICOLON COMMA DOT
%token EOF
%token LIST

(* Types *)
%token BIT BITSTRING INTEGER BOOLEAN REAL ENUMERATION ARRAY CONSTANT

%token ASSERT
%token SEE
%token UNKNOWN
%token UNDEFINED
%token UNPREDICTABLE
%token IMPLEMENTATION_DEFINED
%token SUBARCHITECTURE_DEFINED

(*
 * TODO: Implement this and test.
 * Precedence directives: (example)
 * Weaker are specified first.
 *
 * %nonassoc IF
 * %nonassoc ELSE
 * %nonassoc EQ NEQ
 * %left PLUS MINUS
 * %left TIMES DIV
 * %right EXP
 *)

(* Declare a start symbol. This will be the entry point of our grammar and can be called with Parser.program *)
%start expression program
%type <Ast.definition list> program
%type <Ast.expression> expression


(* End of declarations. *)
%%

(* ------------------------ Expressions ------------------------ *)
argument_expression_list:
    | assignment_expression                                         { [$1] }
    | argument_expression_list COMMA assignment_expression          { $1 @ [$3] }

paren_comma_expression:
    | LPAREN RPAREN                                                 { [] }
    | LPAREN argument_expression_list RPAREN                        { $2 }

primary_expression:
    | IDENTIFIER                                                    { VariableExpression (Identifier $1) }
    | INTEGER_CONSTANT                                              { ConstantExpression (IntegerValue $1) }
    | TRUE                                                          { ConstantExpression (BooleanValue true) }
    | FALSE                                                         { ConstantExpression (BooleanValue false) }
    | LPAREN expression RPAREN                                      { $2 }

postfix_expression:
    | primary_expression                                            { $1 }
    | postfix_expression LBRACK expression RBRACK                   { IndexExpression ($1, $3) }
    | postfix_expression paren_comma_expression                     { FunctionCallExpression ($1, $2) }
    | postfix_expression DOT IDENTIFIER                             { FieldAccessExpression ($1, $3) }

unary_expression:
    | postfix_expression                                            { $1 }
    | PLUS cast_expression                                          { UnaryExpression (PLUS, $2) }
    | MINUS cast_expression                                         { UnaryExpression (MINUS, $2) }
    | BANG cast_expression                                          { UnaryExpression (NOT, $2) }
    | TILDE cast_expression                                         { UnaryExpression (BNOT, $2) }

cast_expression:
    | unary_expression                                              { $1 }

multiplicative_expression:
    | cast_expression                                               { $1 }
    | multiplicative_expression STAR cast_expression                { BinaryExpression (MUL, $1, $3) }
    | multiplicative_expression SLASH cast_expression               { BinaryExpression (FDIV, $1, $3) }
    | multiplicative_expression DIV cast_expression                 { BinaryExpression (IDIV, $1, $3) }
    | multiplicative_expression MOD cast_expression                 { BinaryExpression (MOD, $1, $3) }
    | multiplicative_expression COLON cast_expression               { BinaryExpression (CONCAT, $1, $3) }

additive_expression:
    | multiplicative_expression                                     { $1 }
    | additive_expression PLUS multiplicative_expression            { BinaryExpression (ADD, $1, $3) }
    | additive_expression MINUS multiplicative_expression           { BinaryExpression (SUB, $1, $3) }

shift_expression:
    | additive_expression                                           { $1 }
    | shift_expression LTLT additive_expression                     { BinaryExpression (SHL, $1, $3) }
    | shift_expression GTGT additive_expression                     { BinaryExpression (SHR, $1, $3) }

relational_expression:
    | shift_expression                                              { $1 }
    | relational_expression LT shift_expression                     { BinaryExpression (LT, $1, $3) }
    | relational_expression GT shift_expression                     { BinaryExpression (GT, $1, $3) }
    | relational_expression LEQ shift_expression                    { BinaryExpression (LE, $1, $3) }
    | relational_expression GEQ shift_expression                    { BinaryExpression (GE, $1, $3) }
    | relational_expression IN shift_expression                     { BinaryExpression (IN, $1, $3) }

equality_expression:
    | relational_expression                                         { $1 }
    | equality_expression EQEQ relational_expression                { BinaryExpression (EQ, $1, $3) }
    | equality_expression NEQ relational_expression                 { BinaryExpression (NE, $1, $3) }

bitwise_and_expression:
    | equality_expression                                           { $1 }
    | bitwise_and_expression AND equality_expression                { BinaryExpression (BAND, $1, $3) }

bitwise_xor_expression:
    | bitwise_and_expression                                        { $1 }
    | bitwise_xor_expression EOR bitwise_and_expression             { BinaryExpression (BXOR, $1, $3) }

bitwise_or_expression:
    | bitwise_xor_expression                                        { $1 }
    | bitwise_or_expression OR bitwise_xor_expression               { BinaryExpression (BOR, $1, $3) }

logical_and_expression:
    | bitwise_or_expression                                         { $1 }
    | logical_and_expression AMPAMP bitwise_or_expression           { BinaryExpression (AND, $1, $3) }

logical_xor_expression:
    | logical_and_expression                                        { $1 }
    | logical_xor_expression HAT logical_and_expression             { BinaryExpression (XOR, $1, $3) }

logical_or_expression:
    | logical_xor_expression                                        { $1 }
    | logical_or_expression BARBAR logical_xor_expression           { BinaryExpression (OR, $1, $3) }

conditional_expression:
    | logical_or_expression                                         { $1 }

assignment_expression:
    | conditional_expression                                        { $1 }
    | unary_expression EQ assignment_expression                     { BinaryExpression (ASSIGN, $1, $3) }

expression:
    | assignment_expression                                         { $1 }

constant_expression:
	| conditional_expression                                        { $1 }

(* ------------------------ Statements ------------------------ *)




(* A program consisting of a translation unit. *)
program:
    | translation_unit_file     			                        { $1 }

(* Collection of external declarations. *)
translation_unit_file:
    | external_declaration translation_unit_file                    { [$1] @ $2 }
    | external_declaration EOF                                      { [$1] }

(* Variable/function definitions outside a function. *)
external_declaration:
    | function_definition                                           { $1 }
    | variable_definition                                           { $1 }

(* Definition of a variable. *)
variable_definition:
    | qualified_type IDENTIFIER EQ expression SEMICOLON             { VariableDefinition ($1, $2, Some $4) }
    | qualified_type IDENTIFIER SEMICOLON                           { VariableDefinition ($1, $2, None) }

(* Definition of a function. *)
function_definition:
    | qualified_type IDENTIFIER LPAREN RPAREN block                 { FunctionDefinition ($1, $2, [], $5) }
    | qualified_type IDENTIFIER LPAREN parameter_list RPAREN block  { FunctionDefinition ($1, $2, $4, $6) }

(* Fully qualified type. *)
qualified_type:
    | type_specifier                                                { QualifiedType (None, $1) }
    | type_qualifier type_specifier                                 { QualifiedType (Some $1, $2) }

(* Language types. *)
type_specifier:
    | INTEGER                                                       { TypeInteger }
    | BOOLEAN                                                       { TypeBoolean }
    | REAL                                                          { TypeReal }
    | BIT                                                           { TypeBitString (ConstantExpression (IntegerValue 1)) }
    | ARRAY qualified_type                                          { TypeArray $2 }
    | BITSTRING LPAREN expression RPAREN                            { TypeBitString $3 }

(* Qualifiers for types, we only support const. *)
type_qualifier:
    | CONSTANT                                                      { TypeQualifierConstant }

(* List of function parameters separated by a comma. *)
parameter_list:
    | parameter_declaration                                         { [$1] }
    | parameter_list COMMA parameter_declaration                    { $1 @ [$3] }

parameter_declaration:
    | qualified_type IDENTIFIER                                     { FunctionParameter ($1, $2) }

// enum_list:
//     | IDENTIFIER                                                    { [$1] }
//     | enum_list COMMA IDENTIFIER                                    { $1 @ [$3] }

block:
    | LBRACE block_element_list RBRACE                              { Program $2 }

block_element_list:
    |                                                               { [] }
    | variable_definition block_element_list                        { [ DefinitionStatement $1 ] @ $2 }
    | statement block_element_list                                  { [$1] @ $2 }

case_statement:
    | CASE constant_expression OF case_block ENDCASE                { CaseStatement ($2, $4) }

case_block:
    |                                                               { [] }
    | WHEN constant_expression statement_list case_block            { [WhenStatement ($2, $3)] @ $4 }
    | OTHERWISE statement_list case_block                           { [OtherwiseStatement ($2)] @ $3 }

statement:
    | case_statement                                                { $1 }
    | FOR expression TO expression statement_list ENDFOR            { ForStatement ($2, $4, $5) }
    | REPEAT statement_list UNTIL expression                        { RepeatStatement ($2, $4) }
    | RETURN SEMICOLON                                              { ReturnStatement (EmptyExpression) }
    | RETURN expression SEMICOLON                                   { ReturnStatement ($2) }
    | IF expression THEN statement_list ENDIF                       { IfStatement ($2, $4, []) }
    | IF expression THEN statement_list ELSE statement_list ENDIF   { IfStatement ($2, $4, $6) }

statement_list:
    | statement                                                     { [$1] }
    | statement_list statement                                      { $1 @ [$2] }