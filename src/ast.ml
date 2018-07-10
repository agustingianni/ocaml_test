type identifier =
  | Identifier of string

and type_specifier =
  | TypeInteger
  | TypeBoolean
  | TypeReal
  | TypeBitString of expression
  | TypeArray of qualified_type
  | TypeStruct of string * (qualified_type * string) list
  | TypeEnumeration of string * (string * expression) list

and type_qualifier =
  | TypeQualifierConstant

and qualified_type =
  | QualifiedType of type_qualifier option * type_specifier

and unary_operator =
  | PLUS
  | MINUS
  | NOT
  | BNOT

and binary_operator =
  | ADD
  | SUB
  | MUL
  | IDIV
  | FDIV
  | MOD
  | AND
  | OR
  | XOR
  | BAND
  | BOR
  | BXOR
  | EQ
  | NE
  | LT
  | GT
  | LE
  | GE
  | ASSIGN
  | CONCAT
  | SHL
  | SHR
  | IN

and expression =
  | EmptyExpression
  | UnaryExpression of unary_operator * expression
  | BinaryExpression of binary_operator * expression * expression
  | FunctionCallExpression of expression * expression list
  | IndexExpression of expression * expression
  | FieldAccessExpression of expression * string
  | ConstantExpression of constant
  | VariableExpression of identifier

and constant =
  | BooleanValue of bool
  | IntegerValue of int

and definition =
  | VariableDefinition of qualified_type * string * expression option
  | FunctionDefinition of qualified_type * string * function_parameter list * statement

and statement =
  | EmptyStatement
  | ExpressionStatement of expression
  | CompoundStatement of statement list
  | BreakStatement
  | IfStatement of expression * statement * statement
  | CaseStatement of expression * statement
  | WhenStatement of expression * statement
  | OtherwiseStatement of statement
  | ForStatement of expression * expression * statement
  | RepeatStatement of statement * expression
  | ReturnStatement of expression
  | DefinitionStatement of definition

and types =
  | Enumeration of string * string list
  | BitString of string * int

and program =
  | Program of statement list

and function_parameter =
  | FunctionParameter of qualified_type * string

let unary_operator_to_string operator =
  match operator with
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | NOT -> "NOT"
  | BNOT -> "BNOT"

let binary_operator_to_string operator =
  match operator with
  | ADD -> "ADD"
  | SUB -> "SUB"
  | MUL -> "MUL"
  | IDIV -> "IDIV"
  | FDIV -> "FDIV"
  | MOD -> "MOD"
  | AND -> "AND"
  | OR -> "OR"
  | XOR -> "XOR"
  | BAND -> "BAND"
  | BOR -> "BOR"
  | BXOR -> "BXOR"
  | EQ -> "EQ"
  | NE -> "NE"
  | LT -> "LT"
  | GT -> "GT"
  | LE -> "LE"
  | GE -> "GE"
  | ASSIGN -> "ASSIGN"
  | CONCAT -> "CONCAT"
  | SHL -> "SHL"
  | SHR -> "SHR"
  | IN -> "IN"

let rec expression_to_string expression = match expression with
  | EmptyExpression  -> "EmptyExpression"
  | UnaryExpression (o, e)  -> unary_expression_to_string o e
  | BinaryExpression (o, e0, e1)  -> binary_expression_to_string o e0 e1
  | FunctionCallExpression (e0, el1) -> function_call_expression_to_string e0 el1
  | IndexExpression (e0, e1) -> index_expression_to_string e0 e1
  | FieldAccessExpression (e, n) -> field_access_expression_to_string e n
  | ConstantExpression (value) -> constant_expression_to_string value
  | VariableExpression (i) -> variable_expression_to_string i

and unary_expression_to_string operator expression =
  Printf.sprintf "UnaryExpression { %s %s }"
    (unary_operator_to_string operator)
    (expression_to_string expression)

and binary_expression_to_string operator expression0 expression1 =
  Printf.sprintf "BinaryExpression { %s %s %s }"
    (binary_operator_to_string operator)
    (expression_to_string expression0)
    (expression_to_string expression1)

and function_call_expression_to_string expression0 expression1 =
  Printf.sprintf "FunctionCallExpression { %s %s }"
    (expression_to_string expression0)
    "(expression_to_string expression1)"

and index_expression_to_string expression0 expression1 =
  Printf.sprintf "IndexExpression { %s %s }"
    (expression_to_string expression0)
    (expression_to_string expression1)

and field_access_expression_to_string expression field_name =
  Printf.sprintf "FieldAccessExpression { %s %s }"
    (expression_to_string expression)
    field_name

and constant_expression_to_string expression = match expression with
  | BooleanValue (value) -> Printf.sprintf "BooleanValue { %b }" value
  | IntegerValue (value) -> Printf.sprintf "IntegerValue { %u }" value

and variable_expression_to_string identifier = match identifier with
  | Identifier(name) ->  Printf.sprintf "VariableExpression { %s }" name
