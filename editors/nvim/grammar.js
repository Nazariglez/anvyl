// helpers
function commaSep(rule) {
  return optional(commaSep1(rule));
}
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)), optional(','));
}
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

module.exports = grammar({
  name: 'anvyx',

  extras: $ => [/\s/, $.line_comment],

  word: $ => $.identifier,

  conflicts: $ => [
    [$._expression, $._pattern],
    [$._expression, $.literal_pattern],
    [$._expression, $.lambda_expression],
    [$._pattern],
    [$.defer_statement, $._expression],
    [$.field_initializer, $.struct_pattern],
    [$.field_initializer, $.enum_pattern],
    [$.field_initializer, $.inferred_enum_pattern],
    [$.argument_list, $.inferred_enum_pattern],
    [$.range_pattern, $.rest_pattern],
  ],

  rules: {
    // top-level
    source_file: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.function_definition,
      $.struct_definition,
      $.enum_definition,
      $.extend_block,
      $.import_statement,
      $.extern_declaration,
      $.contract_definition,
      $.type_alias_definition,
      $.const_declaration,
      $.global_declaration,
    ),

    // comments
    doc_comment: $ => token(prec(2, seq('///', /[^\n]*/))),
    line_comment: $ => token(prec(1, seq('//', /[^\n]*/))),

    // identifiers
    identifier: $ => /[a-zA-Z_][a-zA-Z0-9_]*/,

    // literals
    integer_literal: $ => token(choice(
      /0[xX][0-9a-fA-F][0-9a-fA-F_]*/,
      /0[bB][01][01_]*/,
      /0[oO][0-7][0-7_]*/,
      /[0-9][0-9_]*/,
    )),

    float_literal: $ => {
      const DEC = /[0-9][0-9_]*/;
      return token(prec(2, choice(
        seq(DEC, '.', DEC, optional(/[eE][+-]?[0-9][0-9_]*/), optional(/[fd]/)),
        seq(DEC, /[eE][+-]?[0-9][0-9_]*/, optional(/[fd]/)),
        seq(DEC, /[fd]/),
      )));
    },

    string_literal: $ => seq(
      '"',
      repeat(choice($.escape_sequence, alias($.string_content, $.string_content))),
      '"',
    ),
    string_content: $ => token.immediate(prec(-1, /[^"\\]+/)),
    escape_sequence: $ => token.immediate(seq('\\', choice('n', 't', 'r', '\\', '"', '{', '}'))),

    interpolated_string: $ => seq(
      'f"',
      repeat(choice(
        $.escape_sequence,
        $.interpolation,
        alias($.interpolated_string_content, $.string_content),
      )),
      '"',
    ),
    interpolated_string_content: $ => token.immediate(prec(-1, /[^"\\{]+/)),

    interpolation: $ => seq(
      token.immediate('{'),
      $._expression,
      optional(seq(':', $.format_specifier)),
      '}',
    ),
    format_specifier: $ => token.immediate(/[^}]+/),

    // annotations
    annotation: $ => seq(
      '@',
      field('name', $.identifier),
      optional($.annotation_arguments),
    ),
    annotation_arguments: $ => seq(
      '(',
      commaSep(choice(
        $.string_literal,
        seq($.identifier, optional(seq('=', $._annotation_value))),
      )),
      ')',
    ),
    _annotation_value: $ => choice(
      $.string_literal, $.integer_literal, $.float_literal, 'true', 'false',
    ),

    // visibility
    visibility_modifier: $ => 'pub',

    // function definition
    function_definition: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'fn',
      field('name', $.identifier),
      optional($.type_parameters),
      $.parameter_list,
      optional($.return_type),
      $.block,
    ),

    type_parameters: $ => seq('<', commaSep1($.generic_parameter), '>'),
    generic_parameter: $ => choice($.type_parameter, $.const_parameter),
    type_parameter: $ => $.identifier,
    const_parameter: $ => seq($.identifier, ':', 'int'),

    parameter_list: $ => seq('(', commaSep(choice($.method_receiver, $.parameter)), ')'),
    method_receiver: $ => seq(optional('var'), 'self'),
    parameter: $ => seq(
      optional('var'),
      field('name', $.identifier),
      ':',
      optional('as'),
      $._type,
      optional(seq('=', $._expression)),
    ),
    return_type: $ => prec.right(seq('->', optional('var'), $._type)),

    // struct definition
    struct_definition: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      field('keyword', choice('struct', 'dataref')),
      field('name', $.identifier),
      optional($.type_parameters),
      $.struct_body,
    ),
    struct_body: $ => seq('{', commaSep($.struct_field), '}'),
    struct_field: $ => seq(
      repeat($.doc_comment),
      repeat($.annotation),
      optional($.visibility_modifier),
      optional('embed'),
      field('name', $.identifier),
      ':',
      $._type,
      optional($.embed_selector),
      optional(seq('=', $._expression)),
    ),
    embed_selector: $ => seq('{', commaSep1($.embed_selector_item), '}'),
    embed_selector_item: $ => seq(optional('fn'), $.identifier, optional(seq('as', $.identifier))),

    // enum definition
    enum_definition: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'enum',
      field('name', $.identifier),
      optional($.type_parameters),
      $.enum_body,
    ),
    enum_body: $ => seq('{', commaSep($.enum_variant), '}'),
    enum_variant: $ => seq(
      repeat($.doc_comment),
      repeat($.annotation),
      field('name', $.identifier),
      optional(choice(
        seq('(', commaSep($._type), ')'),
        seq('{', commaSep($.struct_field), '}'),
      )),
    ),

    // contract definition
    contract_definition: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'contract',
      field('name', $.identifier),
      optional($.type_parameters),
      '{',
      repeat($.contract_requirement),
      '}',
    ),
    contract_requirement: $ => choice(
      seq($.contract_ref, ';'),
      seq(repeat($.doc_comment), 'fn', field('name', $.identifier), $.parameter_list, optional($.return_type), ';'),
    ),
    contract_ref: $ => prec.left(sep1($.contract_ref_item, '+')),
    contract_ref_item: $ => prec.left(sep1($.identifier, '.')),

    type_alias_definition: $ => seq(
      optional($.visibility_modifier),
      'type', field('name', $.identifier), optional($.type_parameters), '=', $._type, ';',
    ),

    // extend block
    extend_block: $ => seq(
      optional($.visibility_modifier),
      'extend',
      optional($.type_parameters),
      $._type,
      '{',
      repeat(choice($.function_definition, $.cast_from_declaration)),
      '}',
    ),
    cast_from_declaration: $ => seq(
      'cast', 'from', $.parameter_list, $.return_type, $.block,
    ),

    // import statement
    import_statement: $ => seq(
      optional($.visibility_modifier),
      'import',
      $.import_path,
      choice(
        seq('as', $.identifier, ';'),
        seq('{', choice('*', commaSep1($.import_item)), '}', ';'),
        ';',
      ),
    ),
    import_path: $ => sep1($.identifier, '.'),
    import_item: $ => seq($.identifier, optional(seq('as', $.identifier))),

    // extern declaration
    extern_declaration: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'extern',
      choice(
        seq('fn', field('name', $.identifier), $.parameter_list, optional($.return_type), ';'),
        seq('type', field('name', $.identifier), optional($.extern_representation), choice(
          seq('{', repeat($._extern_member), '}'),
          ';',
        )),
      ),
    ),
    extern_representation: $ => seq('rep', choice('shared', 'inline')),
    _extern_member: $ => choice(
      $.extern_field,
      $.extern_init,
      $.extern_method,
      $.extern_operator,
    ),
    extern_field: $ => seq(optional('computed'), field('name', $.identifier), ':', $._type, ';'),
    extern_init: $ => seq('init', optional($.extern_init_parameter_list), ';'),
    extern_init_parameter_list: $ => seq('(', commaSep($.parameter), ')'),
    extern_method: $ => seq('fn', field('name', $.identifier), $.extern_parameter_list, optional($.return_type), ';'),
    extern_parameter_list: $ => seq('(', commaSep($.extern_parameter), ')'),
    extern_parameter: $ => choice(seq('shared', 'self'), seq(optional('var'), 'self'), $.parameter),
    extern_operator: $ => seq('op', choice(
      seq($._type, choice('+', '-', '*', '/', '%', '==', '!=', '<', '<=', '>', '>='), $._type),
      seq('-', $._type),
    ), $.return_type, ';'),

    // const declaration
    const_declaration: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'const',
      field('name', $.identifier),
      optional(seq(':', $._type)),
      '=',
      $._expression,
      ';',
    ),

    // runtime global declaration
    global_declaration: $ => seq(
      repeat($.annotation),
      repeat($.doc_comment),
      optional($.visibility_modifier),
      'lazy',
      choice('let', 'var'),
      field('name', $.identifier),
      optional(seq(':', $._type)),
      '=',
      $._expression,
      ';',
    ),

    // variable declaration
    variable_declaration: $ => seq(
      choice('let', 'var'),
      $._pattern,
      optional(seq(':', $._type)),
      '=',
      $._expression,
      optional(seq('else', $.block)),
      ';',
    ),

    // control flow
    if_expression: $ => prec.right(seq(
      'if',
      optional(seq('let', $._pattern, '=')),
      field('condition', $._expression),
      $.block,
      optional(seq('else', choice($.if_expression, $.block))),
    )),

    while_expression: $ => seq(
      'while',
      choice(
        seq('let', $._pattern, '=', $._expression),
        $._expression,
      ),
      $.block,
    ),

    for_expression: $ => seq(
      'for', $._pattern, 'in',
      optional('rev'),
      $._expression,
      optional(seq('step', $._expression)),
      $.block,
    ),

    match_expression: $ => seq(
      'match', $._expression,
      '{', commaSep($.match_arm), '}',
    ),
    match_arm: $ => seq(
      $._pattern,
      '=>',
      $._expression,
    ),

    return_statement: $ => seq('return', optional($._expression), ';'),
    break_statement: $ => seq('break', ';'),
    continue_statement: $ => seq('continue', ';'),
    defer_statement: $ => seq('defer', choice($.block, seq($._expression, ';'))),

    // expressions
    _expression: $ => choice(
      $.identifier,
      $.self_expression,
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      $.interpolated_string,
      'true',
      'false',
      'nil',
      $.try_expression,
      $.unary_expression,
      $.binary_expression,
      $.call_expression,
      $.field_expression,
      $.index_expression,
      $.cast_expression,
      $.assignment_expression,
      $.ternary_expression,
      $.range_expression,
      $.block,
      $.if_expression,
      $.match_expression,
      $.while_expression,
      $.for_expression,
      $.lambda_expression,
      $.struct_literal,
      $.array_literal,
      $.map_literal,
      $.parenthesized_expression,
      $.tuple_expression,
      $.inferred_enum_expression,
    ),

    self_expression: $ => 'self',

    try_expression: $ => prec.right(14, seq('try', $._expression)),
    unary_expression: $ => prec.left(14, seq(choice('-', '!', '~'), $._expression)),

    binary_expression: $ => {
      const table = [
        [prec.left, 12, choice('*', '/', '%')],
        [prec.left, 11, choice('+', '-')],
        [prec.left, 10, choice('<<', '>>')],
        [prec.left, 9, choice('<', '>', '<=', '>=')],
        [prec.left, 8, choice('==', '!=')],
        [prec.left, 7, '&'],
        [prec.left, 6, '^'],
        [prec.left, 5, '|'],
        [prec.left, 4, '&&'],
        [prec.left, 3, '||'],
        [prec.left, 2, '??'],
      ];
      return choice(...table.map(([fn, p, op]) => fn(p, seq(
        field('left', $._expression), field('operator', op), field('right', $._expression)
      ))));
    },

    assignment_expression: $ => prec.right(1, seq(
      $._expression,
      choice('=', '+=', '-=', '*=', '/=', '^=', '&=', '|=', '<<=', '>>='),
      $._expression,
    )),

    ternary_expression: $ => prec.right(2, seq(
      $._expression, '?', $._expression, ':', $._expression,
    )),

    cast_expression: $ => prec.left(13, seq($._expression, choice('as', 'as?'), $._type)),

    call_expression: $ => prec.left(16, seq(
      field('function', $._expression),
      choice($.argument_list, $.optional_argument_list),
    )),
    argument_list: $ => seq('(', commaSep($._expression), ')'),
    optional_argument_list: $ => seq('?(', commaSep($._expression), ')'),

    field_expression: $ => prec.left(16, seq(
      $._expression, choice('.', '?.'), field('field', $.identifier),
    )),

    index_expression: $ => prec.left(16, seq(
      $._expression, choice(seq('[', $._expression, ']'), seq('?[', $._expression, ']')),
    )),

    range_expression: $ => prec.left(10, seq(
      $._expression, choice('..', '..='), $._expression,
    )),

    parenthesized_expression: $ => seq('(', $._expression, ')'),
    tuple_expression: $ => seq('(', $._expression, ',', commaSep($._expression), ')'),

    lambda_expression: $ => seq(
      choice(seq('|', commaSep($.lambda_parameter), '|'), '||'),
      optional(seq('->', $._type)),
      choice($.block, $._expression),
    ),
    lambda_parameter: $ => seq(
      optional('var'), $.identifier, optional(seq(':', optional('as'), $._type)),
    ),

    struct_literal: $ => prec(-1, seq(
      $.identifier,
      optional(seq('.', $.identifier)),
      '{', commaSep($.field_initializer), '}',
    )),
    field_initializer: $ => choice(
      seq($.identifier, ':', $._expression),
      $.identifier,
    ),

    array_literal: $ => choice(
      seq('[', $._expression, ';', $._expression, ']'),
      seq('[', commaSep($._expression), ']'),
    ),
    map_literal: $ => seq(
      '[', choice(
        seq(':', ']'),
        seq(sep1(seq($._expression, ':', $._expression), ','), optional(','), ']'),
      ),
    ),

    inferred_enum_expression: $ => prec.left(17, choice(
      seq('.', $.identifier),
      seq('.', $.identifier, $.argument_list),
      seq('.', $.identifier, '{', commaSep($.field_initializer), '}'),
    )),

    block: $ => seq(
      '{',
      repeat($._statement),
      optional($._expression),
      '}',
    ),

    // statements
    _statement: $ => choice(
      $.variable_declaration,
      $.const_declaration,
      $.function_definition,
      $.return_statement,
      $.break_statement,
      $.continue_statement,
      $.defer_statement,
      $.expression_statement,
    ),
    expression_statement: $ => seq($._expression, ';'),

    // types
    _type: $ => choice(
      $.builtin_type,
      $.slice_type,
      $.type_identifier,
      $.generic_type,
      $.optional_type,
      $.function_type,
      $.dynamic_type,
      $.array_type,
      $.list_type,
      $.map_type,
      $.tuple_type,
      seq('(', $._type, ')'),
    ),
    builtin_type: $ => choice('int', 'float', 'bool', 'string', 'void', 'any'),
    slice_type: $ => seq('slice', '[', $._type, ']'),
    type_identifier: $ => $.identifier,
    generic_type: $ => prec(1, seq($.identifier, '<', commaSep1($._type), '>')),
    optional_type: $ => prec.left(seq($._type, '?')),
    function_type: $ => prec.right(seq(
      'fn', '(', commaSep(seq(optional('var'), optional('escaping'), $._type)), ')', optional($.return_type),
    )),
    dynamic_type: $ => seq('dyn', choice('_', $.contract_ref)),
    array_type: $ => seq(
      '[', $._type, ';', choice($.integer_literal, $.identifier, '_'), ']',
    ),
    list_type: $ => seq('[', $._type, ']'),
    map_type: $ => seq('[', $._type, ':', $._type, ']'),
    tuple_type: $ => seq('(', $._type, ',', commaSep1($._type), ')'),

    // patterns
    _pattern: $ => choice(
      $.identifier,
      '_',
      $.literal_pattern,
      $.struct_pattern,
      $.enum_pattern,
      $.inferred_enum_pattern,
      $.tuple_pattern,
      seq('var', $.identifier),
      'nil',
      $.range_pattern,
      $.or_pattern,
      $.rest_pattern,
      seq(optional('('), $._pattern, '?', optional(')')),
    ),
    literal_pattern: $ => choice(
      $.integer_literal,
      $.float_literal,
      $.string_literal,
      seq('-', $.integer_literal),
      seq('-', $.float_literal),
      'true',
      'false',
    ),
    struct_pattern: $ => seq(
      $.identifier,
      '{',
      commaSep(choice(
        seq($.identifier, ':', $._pattern),
        $.identifier,
      )),
      '}',
    ),
    enum_pattern: $ => seq(
      $.identifier,
      '.',
      $.identifier,
      optional(choice(
        seq('(', commaSep($._pattern), ')'),
        seq('{', commaSep(choice(seq($.identifier, ':', $._pattern), $.identifier)), optional('..'), '}'),
      )),
    ),
    inferred_enum_pattern: $ => seq(
      '.',
      $.identifier,
      optional(choice(
        seq('(', commaSep($._pattern), ')'),
        seq('{', commaSep(choice(seq($.identifier, ':', $._pattern), $.identifier)), optional('..'), '}'),
      )),
    ),
    tuple_pattern: $ => seq('(', $._pattern, ',', commaSep($._pattern), ')'),
    or_pattern: $ => prec.left(seq($._pattern, '|', $._pattern)),
    range_pattern: $ => prec.left(seq(
      optional($._expression), choice('..', '..='), optional($._expression),
    )),
    rest_pattern: $ => '..',
  },
});
