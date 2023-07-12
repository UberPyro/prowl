: range z z -- {r* -- r* z}
= range
  dup!! (LO HI)~ (<=)
    LO (HI - 1) range [HI]
  lin

= main main
