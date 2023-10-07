= rules
  3 "fizz", 
  5 "buzz", 
  7 "bizz"

= div divmod nip (== 0)

= fizzbuzz
  (1 ..) [parse] sip [rules bury div gen~] cons enq~ nip nip elim

/*
= fizzbuzz
  (1 ..) dup [rules bury div gen~] cons enq~ (parse || nip nip) */
