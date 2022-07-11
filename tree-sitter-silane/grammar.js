const id_tail = "(\-?[A-Za-z0-9_\'])*";
const sym = "[~!@#$%^&*\\-=+\\.?:<>|/\\\\]+";
const suffix = "[?+*!]|!!";
const comment = /([^*\/]|\*[^\/]|\/[^*]|[ \t\n])*[^*\/]/;

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

    symbol: $ => new RegExp(sym), 
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
      seq("(", $.symbol, ")"), 
      seq("{", $.symbol, "}"),
      seq("{", "and", token.immediate(new RegExp(sym)), "}"),
    ),

    // _modtype: $ => choice("sig", "mix"),

    // Type Language

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
      repeat($._head), 
      "--", 
      optional(seq($._span, ",")), 
      repeat($._head), 
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
