const id_tail = "(\-?[A-Za-z0-9_\'])*";
const sym = "[~!@#$%^&*\\-=+\\.?:<>|/\\\\]+";
const suffix = "[?+*!]|!!";
const comment = /([^*\/]|\*[^\/]|\/[^*]|[ \t\n])*[^*\/]/;

const ops = [];

module.exports = grammar({
  name: 'silane',

  extras: $ => [
    /\s/, 
    $.comment, 
  ], 

  word: $ => $.id, 

  rules: {
    source_file: $ => optional($.type), 

    quant: $ => token.immediate(/[?+*][?+]?/),
    check: $ => token.immediate("!"),
    id: $ => new RegExp("[a-z]" + id_tail),
    stack_id: $ => new RegExp("[A-Z]" + id_tail),
    opt_id: $ => new RegExp("@[a-z]" + id_tail),
    cap: $ => new RegExp("[A-Z]" + id_tail),
    exn: $ => new RegExp("[A-Z]" + id_tail + "\\!"),

    custom_symbol: $ => new RegExp(sym), 
    symbol: $ => choice($.custom_symbol, ...ops), 
    blank: $ => new RegExp("_" + id_tail),
    int: $ => /0|[1-9][0-9]*/, 
    float: $ => /[1-9]+\.[0-9]*|\.[0-9]+/, 
    string: $ => seq('"', repeat($.string_content), '"'),
    char: $ => seq("'", optional($.character_content), "'"),

    string_content: $ => choice(
      token.immediate(' '),
      token.immediate('\n'),
      token.immediate('\t'),
      /[^\\"]+/,
      $.escape_sequence
    ),

    character_content: $ => choice(
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
    bound: $ => choice(
      $.int, 
      $.id, 
      prec.left(1, seq($.bound, "*", $.bound)), 
      prec.left(0, seq($.bound, "+", $.bound)), 
    ),

    span: $ => seq("{", span($.bound), "}"),

    param: $ => choice(
      $.cap, 
      $.int, 
      $.span, 
    ),

    name: $ => choice(
      $.id, 
      seq("{", $.symbol, "}"), 
    ),

    common_type: $ => choice(
      "int", 
      "float", 
      "string", 
      "char", 
      "pair", 
      "choice", 
    ),

    arg: $ => choice(
      $.stack, 
      $.head, 
      $.span, 
    ),

    tail: $ => choice(
      $.int, 
      ".", 
    ),

    head: $ => choice(
      $.common_type, 
      $.id, 
      $.cap, 
      seq($.head, "<", sep1(",", $.arg), ">"), 
      seq("[", $.type, "]"), 
      seq("%[", $.type, "]"), 
      // seq("(", "sig", $.mod, ")"), 
      // seq("(", "mod", $.mod, ")"), 
      // seq("{", $.cap, ":", $.mod, ")"), 
      seq($.opt_id, optional(seq("::", $.id))), 
      seq("#[", sep1(",", choice($.span, $.int)), ";", $.type, "]"), 
      prec.left(0, seq($.cap, ".", $.head)), 
    ),

    stack: $ => choice(
      seq($.tail, repeat($.head)), 
      prec.left(1, seq($.stack, "&", $.stack)), 
      prec.left(0, seq($.stack, "|", $.stack)), 
      seq("(", $.stack, ")"), 
    ),

    type: $ => seq(
      $.stack, 
      choice("--", "-?", "-+", "-*"), 
      $.stack, 
    ),
    
  }

});

function span(rule) {
  return seq(
    optional(rule), 
    optional(seq(":", optional(rule)))
  )
};

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
