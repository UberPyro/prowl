: range z z -- {r* -- r* z}
= range
  dup!! (LO HI)~ (>)
    (LO + 1) HI range [LO]
  lin

= main main
