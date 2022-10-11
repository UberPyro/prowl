const id_tail = "(\-?[A-Za-z0-9_\'])*";
const sym = "[~!@#$%^&*\\-=+\\.?:<>|/\\\\]*";
const comment = /([^*\/]|\*[^\/]|\/[^*]|[ \t\n])*[^*\/]/;

module.exports = grammar({
  name: 'prowl',

  extras: $ => [
    /\s/, 
    $.comment, 
  ], 

  word: $ => $.id, 

  rules: {
    source_file: $ => optional($.expr), 

    id: $ => new RegExp("[a-z]" + id_tail), 
    int: $ => /0|[1-9][0-9]*/, 
    string: $ => seq('"', repeat($.string_content), '"'),
    char: $ => seq("'", optional($.character_content), "'"),

    uop: $ => new RegExp("[~!?]" + sym), 
    op1: $ => new RegExp("[$&|=]" + sym), 
    op2: $ => new RegExp("[@:]" + sym), 
    op3: $ => new RegExp("[+\\-]" + sym), 
    op4: $ => new RegExp("[*\\/%]" + sym), 
    op5: $ => new RegExp("[\\.\\^#]" + sym), 

    bop: $ => choice($.op1, $.op2, $.op3, $.op4, $.op5), 

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

    expr: $ => choice(
      repeat1($.word), 
      prec.left(6, seq($.expr, $.op5, $.expr)), 
      prec(5, seq($.expr, $.uop)), 
      prec.left(4, seq($.expr, $.op4, $.expr)), 
      prec.left(3, seq($.expr, $.op3, $.expr)), 
      prec.left(2, seq($.expr, $.op2, $.expr)), 
      prec.left(1, seq($.expr, $.op1, $.expr)), 
      prec(0, seq(
        "let", optional("rec"), 
        repeat1($.id), "=", 
        $.expr, "in", 
        $.expr
      )), 
      prec(0, seq(
        "as", repeat1($.id), "->", $.expr
      )), 
      prec.left(-1, seq($.expr, "|", $.expr)),
    ), 

    word: $ => choice(
      $.int, 
      $.char, 
      seq("[", $.expr, "]"), 
      seq("(", $.expr, ")"), 
      seq("{", sep1(",", $.expr), "}"), 
      seq("[", "]"), 
      seq("(", ")"), 
      seq("{", "}"), 
      $.string, 
      $.id, 
      seq("(", $.bop, $.expr, ")"), 
      seq("(", $.expr, $.bop, ")"), 
      seq("(", $.bop, ")"), 
      seq("(", $.uop, ")"), 
      seq("[", $.bop, $.expr, "]"), 
      seq("[", $.expr, $.bop, "]"), 
      seq("[", $.bop, "]"), 
      seq("[", $.uop, "]"), 
    ), 
  }
});

function sep1(delimiter, rule) {
  return seq(
    optional(delimiter), 
    rule, 
    repeat(seq(delimiter, rule)), 
    optional(delimiter)
  )
}