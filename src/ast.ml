open Core.Option
module Option = Core.Option

type identifier =
  | Identifier of string

and type_specifier =
  | TypeInteger
  | TypeBoolean
  | TypeReal
  | TypeBitString of expression
  | TypeArray of qualified_type
  | TypeStruct of string * struct_field list
  | TypeEnumeration of string * enumeration_value list
  | TypeList of list_element list
  | TypeNamed of string

and list_element =
  | ListElement of qualified_type

and struct_field =
  | StructField of qualified_type * string

and enumeration_value =
  | EnumerationValue of string

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
  | RealValue of float

and definition =
  | ArrayDefinition of qualified_type * string * expression
  | VariableDefinition of qualified_type * string * expression option
  | FunctionDefinition of qualified_type * string * function_parameter list * statement
  | TypeDefinition of type_specifier

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

and program =
  | Program of statement list

and function_parameter =
  | FunctionParameter of qualified_type * string

let pp_unary_operator = function
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | NOT -> "NOT"
  | BNOT -> "BNOT"

let pp_binary_operator = function
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | IDIV -> "/"
  | FDIV -> "/"
  | MOD -> "%"
  | AND -> "&"
  | OR -> "|"
  | XOR -> "^"
  | BAND -> "&"
  | BOR -> "|"
  | BXOR -> "^"
  | EQ -> "="
  | NE -> "!="
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="
  | ASSIGN -> "="
  | CONCAT -> ":"
  | SHL -> "<<"
  | SHR -> ">>"
  | IN -> "IN"

let rec pp_expression expression = match expression with
  | EmptyExpression  -> "EmptyExpression"
  | UnaryExpression (o, e)  -> pp_unary_expression o e
  | BinaryExpression (o, e0, e1)  -> pp_binary_expression o e0 e1
  | FunctionCallExpression (e0, el1) -> pp_function_call_expression e0 el1
  | IndexExpression (e0, e1) -> pp_index_expression e0 e1
  | FieldAccessExpression (e, n) -> pp_field_access_expression e n
  | ConstantExpression (value) -> pp_constant value
  | VariableExpression (i) -> pp_variable_expression i

and pp_unary_expression operator expression =
  Printf.sprintf "UnaryExpression { %s %s }"
    (pp_unary_operator operator)
    (pp_expression expression)

and pp_binary_expression operator expression0 expression1 =
  Printf.sprintf "BinaryExpression { %s %s %s }"
    (pp_binary_operator operator)
    (pp_expression expression0)
    (pp_expression expression1)

and pp_function_call_expression expression0 expression1 =
  Printf.sprintf "FunctionCallExpression { %s %s }"
    (pp_expression expression0)
    "(pp_expression expression1)"

and pp_index_expression expression0 expression1 =
  Printf.sprintf "IndexExpression { %s %s }"
    (pp_expression expression0)
    (pp_expression expression1)

and pp_field_access_expression expression field_name =
  Printf.sprintf "FieldAccessExpression { %s %s }"
    (pp_expression expression)
    field_name

and pp_constant expression = match expression with
  | BooleanValue (value) -> Printf.sprintf "BooleanValue { %b }" value
  | IntegerValue (value) -> Printf.sprintf "IntegerValue { %u }" value
  | RealValue (value) -> Printf.sprintf "IntegerValue { %f }" value

and pp_variable_expression identifier = match identifier with
  | Identifier(name) ->  Printf.sprintf "VariableExpression { %s }" name

let to_pairs xs =
  let as_array = Array.of_list xs in
  match Array.length as_array with
  | 0 -> []
  | 1 -> []
  | n -> Array.to_list (Array.init (n-1) (fun i -> as_array.(i), as_array.(i + 1)))

type branch =
  | Left
  | Right
  | Root

let branch_char branch = match branch with
  | Left -> "└"
  | Right -> "┌"
  | Root -> " "

let build_path path =
  let pad_chars = function
  | (Left, Right) -> "│ "
  | (Right, Left) -> "│ "
  | _ -> "  "
  in

  path |> to_pairs |> List.map pad_chars |> String.concat ""

let get_last a = List.hd (List.rev a)

(* Print a horizontal tree of the passed expression tree. *)
let print_tree node =
  let rec print_tree_int node path =
    let branch_corner = branch_char (get_last path) in
    let path_string = build_path path in
    match node with
    | BinaryExpression (op, lhs, rhs) ->
      begin
        let value = (pp_binary_operator op) in
        print_tree_int rhs (path @ [Right]);
        Printf.printf "%s%s─┤ %s\n" path_string branch_corner value;
        print_tree_int lhs (path @ [Left]);
      end

    | ConstantExpression (value) ->
      begin
        match value with
        | BooleanValue (value) -> Printf.printf "%s%s──➤ %b\n" path_string branch_corner value
        | IntegerValue (value) -> Printf.printf "%s%s──➤ %u\n" path_string branch_corner value
        | RealValue (value) -> Printf.printf "%s%s──➤ %f\n" path_string branch_corner value
      end

    | VariableExpression (value) ->
      begin
        match value with
        | Identifier (value) -> Printf.printf "%s%s──➤ %s\n" path_string branch_corner value
      end

    | _ ->
      begin
        Printf.printf "%s%s──➤ %s\n" path_string branch_corner "TODO"
      end

    in

    print_tree_int node [Root]

let rec pp_identifier = function
  | Identifier (name) -> Printf.sprintf "ID(%s)" name

and pp_type_qualifier = function
  | TypeQualifierConstant -> "const"

and pp_type_specifier = function
  | TypeInteger -> "integer"
  | TypeBoolean -> "boolean"
  | TypeReal -> "real"
  | TypeBitString (expression) -> Printf.sprintf "bits(%s)" (pp_expression expression)
  | TypeArray (qualified_type) -> Printf.sprintf "array(%s)" (pp_qualified_type qualified_type)
  | TypeStruct (name, fields) ->
    begin
      Printf.sprintf "struct { %s }"
        (fields |> List.map pp_struct_field |> String.concat ", ")
    end

  | TypeEnumeration (name, values) ->
    begin
      Printf.sprintf "enum { %s }"
        (values |> List.map pp_enumeration_value |> String.concat ", ")
    end

  | TypeList (elements) -> "list"
  | TypeNamed (name) -> Printf.sprintf "TypeNamed name=%s" name

and pp_qualified_type = function
  | QualifiedType (type_qualifier, type_specifier) ->
      Option.value_map type_qualifier ~default:"" ~f:pp_type_qualifier ^ pp_type_specifier type_specifier

and pp_struct_field = function
  | StructField (field_type, field_name) -> Printf.sprintf "%s %s" (pp_qualified_type field_type) field_name

and pp_enumeration_value = function
  | EnumerationValue (name) -> name

and pp_definition = function
  | VariableDefinition (qType, vName, iExpr) ->
    begin
      Printf.printf "VariableDefinition type=%s name=%s init_expr=%s\n"
        (pp_qualified_type qType)
        vName
        (Option.value_map iExpr ~default:"None" ~f:pp_expression)
    end

  | FunctionDefinition (qType,fName,parameters,statements) ->
    begin
      Printf.printf "FunctionDefinition\n";
      Printf.printf "  name -> %s\n" fName;
      Printf.printf "  ret -> %s\n" (pp_qualified_type qType);
      List.iter
        (fun parameter -> Printf.printf "  param -> %s\n" (pp_function_parameter parameter))
        parameters
      ;

      pp_statement statements
    end

  | ArrayDefinition (qType, vName, iExpr) ->
    begin
      Printf.printf "ArrayDefinition type=%s name=%s size=%s\n"
        (pp_qualified_type qType)
        vName
        (pp_expression iExpr)
    end

  | TypeDefinition (type_) ->
    begin
      Printf.printf "TypeDefinition type=%s\n" (pp_type_specifier type_)
    end

and pp_statement = function
  | EmptyStatement -> Printf.printf "emtpy\n"
  | ExpressionStatement (expression) ->
    begin
      Printf.printf "ExpressionStatement\n";
      Printf.printf "%s\n" (pp_expression expression)
    end

  | CompoundStatement (statements) ->
    begin
      List.iter pp_statement statements
    end

  | BreakStatement -> Printf.printf "BreakStatement\n"
  | IfStatement (expression, if_statements, else_statements) ->
    begin
      Printf.printf "IfStatement\n";
      Printf.printf "%s\n" (pp_expression expression);
      pp_statement if_statements;
      pp_statement else_statements
    end

  | CaseStatement (expression, statement) ->
    begin
      Printf.printf "CaseStatement\n";
      Printf.printf "%s\n" (pp_expression expression);
      pp_statement statement
    end

  | WhenStatement (expression, statement) ->
    begin
      Printf.printf "WhenStatement\n";
      Printf.printf "%s\n" (pp_expression expression);
      pp_statement statement
    end

  | OtherwiseStatement (statement) ->
    begin
      Printf.printf "OtherwiseStatement\n";
      pp_statement statement
    end

  | ForStatement (from_expr, to_expr, statement) ->
    begin
      Printf.printf "ForStatement\n";
      Printf.printf "%s\n" (pp_expression from_expr);
      Printf.printf "%s\n" (pp_expression to_expr);
      pp_statement statement
    end

  | RepeatStatement (statement, expression) ->
    begin
      Printf.printf "RepeatStatement\n";
      Printf.printf "%s\n" (pp_expression expression);
      pp_statement statement
    end

  | ReturnStatement (expression) ->
    begin
      Printf.printf "ReturnStatement\n";
      Printf.printf "%s\n" (pp_expression expression)
    end

  | DefinitionStatement (definition) ->
    begin
      Printf.printf "DefinitionStatement\n";
      pp_definition definition
    end

and pp_function_parameter = function
  | FunctionParameter (qType, name) ->
    begin
      Printf.sprintf "FunctionParameter -> name=%s type=%s" name (pp_qualified_type qType);
    end

(* Pretty print starting point for programs. *)
and pp_program program =
  Printf.printf "program\n";
  List.iter pp_definition program