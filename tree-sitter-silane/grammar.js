const id_tail = "(\-?[A-Za-z0-9_])*";
const sym = "[~!@#$%^&*\\-=+\\.?:<>|/\\\\]+";
const suffix = "[?+*!]|!!";
const comment = /([^*\/]|\*[^\/]|\/[^*]|[ \t\n])*[^*\/]/;

const seq_op = [
  ["~", ",", "&","^~", "~^", "%"],
  ["^", "|", ";", ":", "\\"],
  ["=", "?", "!"],
];

const bin_op = [
  [
    ["$"], 
    []
  ], 
  [
    [">>=", "=>>"],
    ["=<<", "<<="],
  ],
  [
    [">=>"], ["=>="], 
    ["<=<"], ["=<="], 
  ],
  [
    [],
    []
  ],
  [
    [
      ">>", "~>", 
      ",,", ",>", 
      "&&", "&>", 
      "^>>", "^~>", 
      ">>^", "~>^", 
      "%%", "%>", 
    ],
    [
      "<<", "<~", 
      "<,", 
      "<&", 
      "^<<", "^<~", 
      "<<^","<~^", 
      "<%", 
    ],
  ],
  [
    [
      "^^", "^>", 
      "||", "|>", 
      ";;", ";>", 
      "::", ":>", 
      "\\\\", "\\>", 
    ],
    [
      "<^", 
      "<|", 
      "<;", 
      "<:", 
      "<\\", 
    ],
  ],
  [
    [
      "==", "=>", 
      "??", "?>", 
      "!!", "!>", 
    ],
    [
      "<?", 
      "<!", 
    ],
  ],
  [
    ["<", "<=", ">", ">=", "!="],
    [],
  ],
  [
    [".."],
    [],
  ],
  [
    ["+", "-"],
    [],
  ],
  [
    ["*", "/"],
    [],
  ],
];

function make_seq_ops(rule) {
  return seq_op.map(arr => choice(...arr))
    .reverse()
    .reduce((acc, s) => sep1(s, acc), rule);
};

function make_bin_ops(selfref) {
  return choice(...bin_op.map((arr, i) =>
    arr[0].map(x => prec.left(i*2, seq(selfref, x, selfref)))
    .concat(arr[1].map(x => prec.right(i*2 + 1, seq(selfref, x, selfref))))
  ));
};

const ops = bin_op.flat().flat()

module.exports = grammar({
  name: 'silane',

  extras: $ => [
    /\s/, 
    $.comment, 
  ], 

  word: $ => $.id, 

  rules: {
    source_file: $ => optional($._type),  // temp

    quant: $ => token.immediate(/[?+*][?+]?/),
    check: $ => token.immediate(/!!?/),
    id: $ => new RegExp("[a-z]" + id_tail),
    lin_id: $ => new RegExp("![a-z]" + id_tail),
    opt_id: $ => new RegExp("@[a-z]" + id_tail),
    cap: $ => new RegExp("[A-Z]" + id_tail),
    exn: $ => new RegExp("[A-Z]" + id_tail + "\\!"),
    err: $ => new RegExp("[A-Z]" + id_tail + "\\!\\!"),
    _exn: $ => choice($.exn, $.err),

    custom_symbol: $ => new RegExp(sym), 
    symbol: $ => choice($.custom_symbol, ...ops), 
    blank: $ => new RegExp("_" + id_tail),
    int: $ => /0|[1-9][0-9]*/, 
    float: $ => /[1-9]\.[0-9]*|\.[0-9]+/, 
    string: $ => seq('"', repeat($._string_content), '"'),
    char: $ => seq("'", optional($._character_content), "'"),

    _string_content: $ => choice(
      token.immediate(' '),
      token.immediate('\n'),
      token.immediate('\t'),
      /[^\\"]+/,
      $.escape_sequence
    ),

    _character_content: $ => choice(
      /[^\\']/,
      $.escape_sequence
    ),

    escape_sequence: $ => choice(
      /\\[\\"'ntbr ]/,
      /\\[0-9][0-9][0-9]/,
      /\\x[0-9A-Fa-f][0-9A-Fa-f]/,
      /\\o[0-3][0-7][0-7]/
    ),

    comment: $ => seq(
      "/*", choice(
        comment, 
        comment, $.comment, comment
      ), "*/"
    ),


    // Type Language
    _bound: $ => choice($.int, $.id),
    _span: $ => choice(
      seq("{", optional($._bound), ":", optional($._bound), "}"),
      seq("{", optional($._bound), "}"),
      "erroneous", "det", "multi", 
      "failure", "semidet", "nondet",
    ),

    _tail: $ => choice($.int, "."),
    param_stack: $ => seq($._tail, repeat($.cap)),
    _param: $ => choice(
      $.cap, 
      seq("[", $.param_stack, "--", $.param_stack, "]"),
      $._span,
    ),

    _name: $ => choice(
      $.id, 
      seq("{", $.symbol, "}"),
      seq("{", "and", token.immediate(new RegExp(sym)), "}"),
    ),

    common_type: $ => choice(
      "int", 
      "float", 
      "string", 
      "char",
    ),

    _head: $ => choice(
      $.common_type,
      $.id, 
      $.cap,
      $.lin_id, 
      $._span, 
      seq("[", $._type, "]"),
      seq("%[", $._type, "]"),
      // seq("{", $._mod, "}"),
      // seq("<", $.cap, ":", $._mod, "}"),
      seq($.opt_id, optional(seq("'", $._head))),
      seq("#[", $._span, ",", $._type, "]"),
      seq($.cap, ".", $._head),
    ),

    simple_type: $ => seq(
      field("lhs", repeat($._head)), 
      "--", 
      optional(seq($._span, ",")), 
      field("rhs", repeat($._head)), 
      repeat($.exn),
    ),

    stack: $ => seq($._tail, repeat($._head)),
    multistack: $ => seq(
      optional(seq($._span, ",")),
      $._tail,
      repeat($._head),
      repeat($.exn),
    ),

    alge_stack: $ => algebra($.alge_stack, $.stack),
    alge_multistack: $ => algebra($.alge_multistack, $.multistack),
    advanced_type: $ => seq($.alge_stack, "--", $.alge_multistack),

    _type: $ => choice(
      $.simple_type,
      $.advanced_type
    ),

    record: $ => seq(
      "{", 
      sep1(",", seq($.id, optional(seq(":", $._type)))),
      "}"
    ),

    tacit: $ => seq(repeat($._head), $.cap),
    pointed: $ => seq($.record, $.cap),
    variant: $ => sep1(";", choice($.tacit, $.pointed)),

    _data: $ => choice(
      $.record,
      $.variant,
    ),

    tag: $ => seq(repeat($._head), $._exn),

    // Expression Language
    _expr: $ => choice(
      $.symbol, 
      seq($.expr_seq, $.symbol), 
      seq($.symbol, $.expr_seq), 
      $.expr_seq, 
      $.binding, 
      seq($.expr_seq, $.binding), 
      seq($.expr_seq, $.symbol, $.binding),
    ),

    

    expr_seq: $ => make_seq_ops($._expr_group),

    _expr_group: $ => choice(
      make_bin_ops($._expr_group),
      prec.left(6, seq($._expr_group, $.custom_symbol, $._expr_group)),
      $._term,
    ),

    _term: $ => choice(
      $.id,

      $.int,
      $.float,
      $.string,
      $.char,

      seq("[", $._term, "]"),
      seq("%[", sep(",", seq($._term, "->", $._term)), "]"),
      // seq("{", $.mod, ":", $.mod, "}"),
      seq("#[", sep(",", sep(";", $._term))),

      // finish
    ),

    // _modtype: $ => choice("sig", "mix"),

  }
});

function sep(delimiter, rule) {
  return optional(sep1(delimiter, rule))
}

function sep1(delimiter, rule) {
  return seq(
    optional(delimiter), 
    rule, 
    repeat(seq(delimiter, rule)), 
    optional(delimiter)
  )
}

function algebra(selfref, rule) {
  return choice(
    rule, 
    prec.left(2, seq(rule, "*", rule)),
    prec.left(1, seq(rule, "+", rule)),
    seq("(", selfref, ")"),
  )
}
