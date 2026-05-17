; -- Comments
(line_comment) @comment @spell
(doc_comment) @comment.documentation @spell

; -- Literals
(integer_literal) @number
(float_literal) @number.float

(string_literal) @string
(interpolated_string) @string
(string_content) @string
(escape_sequence) @string.escape
(format_specifier) @string.special

"true" @boolean
"false" @boolean
"nil" @constant.builtin

; -- String interpolation delimiters
(interpolation
  "{" @punctuation.special
  "}" @punctuation.special)

; -- Types
(builtin_type) @type.builtin
(slice_type
  "slice" @type.builtin)
(type_identifier) @type

(struct_definition
  name: (identifier) @type)
(enum_definition
  name: (identifier) @type)

(embed_selector_item
  "fn" @keyword.function)

(contract_definition
  name: (identifier) @type)

(type_alias_definition
  name: (identifier) @type)

(generic_type
  (identifier) @type)

(type_parameter
  (identifier) @type)

(extern_declaration
  "type" name: (identifier) @type)

(dynamic_type
  "dyn" @type.builtin)

(extern_representation
  ["rep" "shared" "inline"] @keyword.modifier)

; -- Functions
(function_definition
  name: (identifier) @function)

(call_expression
  function: (identifier) @function.call)

(call_expression
  function: (field_expression
    field: (identifier) @function.method.call))

(extern_declaration
  "fn" name: (identifier) @function)

(cast_from_declaration
  "cast" @keyword
  "from" @function.builtin)

(extern_init
  "init" @keyword.function)

(extern_operator
  "op" @keyword.operator)

(extern_method
  name: (identifier) @function)

(extern_parameter
  "shared" @keyword.modifier
  "self" @variable.builtin)

; -- Variables and properties
(self_expression) @variable.builtin

(parameter
  name: (identifier) @variable.parameter)
(lambda_parameter
  (identifier) @variable.parameter)

(field_expression
  field: (identifier) @property)

(struct_field
  name: (identifier) @property)

(field_initializer
  (identifier) @property
  ":")

(enum_variant
  name: (identifier) @constant)

(const_declaration
  name: (identifier) @constant)

(global_declaration
  name: (identifier) @constant)

; -- Annotations
(annotation
  "@" @attribute
  name: (identifier) @attribute)

; -- Keywords
["if" "else" "match"] @keyword.conditional
["while" "for"] @keyword.repeat
"in" @keyword.repeat
"return" @keyword.return
"fn" @keyword.function
"import" @keyword.import
(visibility_modifier) @keyword.modifier
"as" @keyword.operator

["let" "var" "const" "lazy"] @keyword
["struct" "enum" "type" "extend" "extern" "dataref" "contract"] @keyword
["computed" "embed" "escaping" "rev" "step"] @keyword
["break" "continue" "defer" "try"] @keyword

; -- Operators
["+" "-" "*" "/" "%"] @operator
["==" "!=" "<" ">" "<=" ">=" "<<" ">>"] @operator
["&&" "||" "!" "??" "?" "?." "?(" "?[" "as?" "^" "~" "&" "|"] @operator
["->" "=>" ".." "..="] @operator
["=" "+=" "-=" "*=" "/=" "^=" "&=" "|=" "<<=" ">>="] @operator

; -- Punctuation
["(" ")" "{" "}" "[" "]"] @punctuation.bracket
[";" "," ":" "."] @punctuation.delimiter

; -- Modules
(import_path
  (identifier) @module)

(import_item
  (identifier) @type)
