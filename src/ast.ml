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
  | FunctionDefinition of qualified_type * string * function_parameter list * program

and statement =
  | EmptyStatement
  | BreakStatement
  | IfStatement of expression * statement list * statement list
  | CaseStatement of expression * statement list
  | WhenStatement of expression * statement list
  | OtherwiseStatement of statement list
  | ForStatement of expression * expression * statement list
  | RepeatStatement of statement list * expression
  | ReturnStatement of expression
  | DefinitionStatement of definition

and types =
  | Enumeration of string * string list
  | BitString of string * int

and program =
  | Program of statement list

and function_parameter =
  | FunctionParameter of qualified_type * string

let expression_to_string expression =
  match expression with
  | EmptyExpression  -> "EmptyExpression"
  | UnaryExpression (o, e)  -> "UnaryExpression"
  | BinaryExpression (o, e0, e1)  -> "BinaryExpression"
  | FunctionCallExpression (e0, e1) -> "FunctionCallExpression"
  | IndexExpression (e0, e1) -> "IndexExpression"
  | FieldAccessExpression (e, n) -> "FieldAccessExpression"
  | ConstantExpression (c) -> "ConstantExpression"
  | VariableExpression (i) -> "VariableExpression"
