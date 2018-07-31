(* Import needed types from other files. *)
%{
    open Ast
%}

%token <int> INTEGER_CONSTANT
%token <float> REAL_CONSTANT
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
%token EOL
%token IS

(* Types *)
%token BIT BITSTRING INTEGER BOOLEAN REAL ENUMERATION ARRAY CONSTANT TYPE

%token ASSERT
%token SEE
%token UNKNOWN
%token UNDEFINED
%token UNPREDICTABLE
%token IMPLEMENTATION_DEFINED
%token SUBARCHITECTURE_DEFINED

(* Declare a start symbol. This will be the entry point of our grammar and can be called with Parser.program *)
%start test_expression program

(* A program is a list of global definitions, that is variables and functions. *)
%type <Ast.definition list> program

(* We also support evaluating expressions by themselves. *)
%type <Ast.expression> expression
%type <Ast.expression> test_expression

(* End of declarations. *)
%%

(* ------------------------ Expressions ------------------------ *)
unary_op:
    | PLUS                                                          { PLUS }
    | MINUS                                                         { MINUS }
    | BANG                                                          { NOT }
    | TILDE                                                         { BNOT }

shift_op:
    | LTLT                                                          { SHL }
    | GTGT                                                          { SHR }

relational_op:
    | LT                                                            { LT }
    | GT                                                            { GT }
    | LEQ                                                           { LE }
    | GEQ                                                           { GE }
    | IN                                                            { IN }

equality_op:
    | EQEQ                                                          { EQ }
    | NEQ                                                           { NE }

additive_op:
    | PLUS                                                          { ADD }
    | MINUS                                                         { SUB }

multiplicative_op:
    | STAR                                                          { MUL }
    | SLASH                                                         { FDIV }
    | DIV                                                           { IDIV }
    | MOD                                                           { MOD }
    | COLON                                                         { CONCAT }

argument_expression_list:
    | separated_list(COMMA, assignment_expression)                  { $1 }

paren_comma_expression:
    | LPAREN argument_expression_list RPAREN                        { $2 }

primary_expression:
    | IDENTIFIER                                                    { VariableExpression (Identifier $1) }
    | INTEGER_CONSTANT                                              { ConstantExpression (IntegerValue $1) }
    | REAL_CONSTANT                                                 { ConstantExpression (RealValue $1) }
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
    | unary_op cast_expression                                      { UnaryExpression ($1, $2) }

cast_expression:
    | unary_expression                                              { $1 }

multiplicative_expression:
    | cast_expression                                               { $1 }
    | multiplicative_expression multiplicative_op cast_expression   { BinaryExpression ($2, $1, $3) }

additive_expression:
    | multiplicative_expression                                     { $1 }
    | additive_expression additive_op multiplicative_expression     { BinaryExpression ($2, $1, $3) }

shift_expression:
    | additive_expression                                           { $1 }
    | shift_expression shift_op additive_expression                 { BinaryExpression ($2, $1, $3) }

relational_expression:
    | shift_expression                                              { $1 }
    | relational_expression relational_op shift_expression          { BinaryExpression ($2, $1, $3) }

equality_expression:
    | relational_expression                                         { $1 }
    | equality_expression equality_op relational_expression         { BinaryExpression ($2, $1, $3) }

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

test_expression:
    | expression EOF                                                { $1 }

constant_expression:
	| conditional_expression                                        { $1 }

(* ------------------------ Statements ------------------------ *)

(* A program consisting of a translation unit. *)
program:
    | translation_unit EOF    			                            { $1 }

(* Collection of external declarations. *)
translation_unit:
    | list(external_declaration)                                    { $1 }

(* Variable/function/type definitions outside a function. *)
external_declaration:
    | function_definition                                           { $1 }
    | variable_definition                                           { $1 }
    | type_definition                                               { $1 }

(* Custom types. *)
type_definition:
    | struct_definition                                             { TypeDefinition $1 }
    | enumeration_definition                                        { TypeDefinition $1 }

struct_definition:
    | TYPE IDENTIFIER IS LPAREN struct_fields RPAREN                { TypeStruct ($2, $5) }

struct_fields:
    | separated_nonempty_list(COMMA, struct_field)                  { $1 }

struct_field:
    | qualified_type IDENTIFIER                                     { StructField ($1, $2) }

enumeration_definition:
    | ENUMERATION IDENTIFIER LBRACE enumeration_elements RBRACE     { TypeEnumeration ($2, $4) }

enumeration_elements:
    | separated_nonempty_list(COMMA, enumeration_element)           { $1 }

enumeration_element:
    | IDENTIFIER                                                    { EnumerationValue $1 }

(* Definition of a variable. *)
variable_definition:
    | qualified_type init_declarator_list SEMICOLON                 { VariableDefinition ($1, $2) }
    | qualified_type IDENTIFIER LBRACK expression RBRACK SEMICOLON  { ArrayDefinition ($1, $2, $4) }

init_declarator_list:
    | separated_nonempty_list(COMMA, init_declarator)               { $1 }

init_declarator:
    | IDENTIFIER                                                    { VariableDeclarator ($1, EmptyExpression) }
    | IDENTIFIER EQ expression                                      { VariableDeclarator ($1, $3) }

(* Definition of a function. *)
function_definition:
    | qualified_type IDENTIFIER LPAREN parameter_list RPAREN compound_statement  { FunctionDefinition ($1, $2, $4, $6) }

(* Fully qualified type. *)
qualified_type:
    | type_qualifier? type_specifier                                { QualifiedType ($1, $2) }

(* Type specifiers in declarations define the type of a variable or function declaration. *)
type_specifier:
    | INTEGER                                                       { TypeInteger }
    | BOOLEAN                                                       { TypeBoolean }
    | REAL                                                          { TypeReal }
    | BITSTRING LPAREN expression RPAREN                            { TypeBitString $3 }
    | BIT                                                           { TypeBitString (ConstantExpression (IntegerValue 1)) }
    | ARRAY qualified_type                                          { TypeArray $2 }
    | LPAREN list_elements RPAREN                                   { TypeList $2 }
    | IDENTIFIER                                                    { TypeNamed $1 }

list_elements:
    | separated_nonempty_list(COMMA, list_element)                  { $1 }

list_element:
    | qualified_type                                                { ListElement $1 }

(* Qualifiers for types, we only support const. *)
type_qualifier:
    | CONSTANT                                                      { TypeQualifierConstant }

(* List of function parameters separated by a comma. *)
parameter_list:
    | separated_list(COMMA, parameter_declaration)                  { $1 }

parameter_declaration:
    | qualified_type IDENTIFIER                                     { FunctionParameter ($1, $2) }

block_element:
    | variable_definition                                           { DefinitionStatement $1 }
    | type_definition                                               { DefinitionStatement $1 }
    | statement                                                     { $1 }

block_element_list:
    | list(block_element)                                           { $1 }

labeled_statement:
    | WHEN constant_expression statement_list                       { WhenStatement ($2, $3) }
    | OTHERWISE statement_list                                      { OtherwiseStatement $2 }

compound_statement:
    | LBRACE block_element_list RBRACE                              { $2 }

expression_statement:
    | expression SEMICOLON                                          { ExpressionStatement $1 }

selection_statement:
    | IF expression THEN statement_list ENDIF                       { IfStatement ($2, $4, []) }
    | IF expression THEN statement_list ELSE statement_list ENDIF   { IfStatement ($2, $4, $6) }
    | CASE expression OF statement_list ENDCASE                     { CaseStatement ($2, $4) }

iteration_statement:
    | FOR expression TO expression statement_list ENDFOR            { ForStatement ($2, $4, $5) }
    | REPEAT statement_list UNTIL expression                        { RepeatStatement ($2, $4) }

jump_statement:
    | RETURN SEMICOLON                                              { ReturnStatement EmptyExpression }
    | RETURN expression SEMICOLON                                   { ReturnStatement $2 }

statement:
    | expression_statement                                          { $1 }
    | labeled_statement                                             { $1 }
    | selection_statement                                           { $1 }
    | iteration_statement                                           { $1 }
    | jump_statement                                                { $1 }

statement_list:
    | list(statement)                                               { $1 }