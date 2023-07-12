= snoc (X Y)~ gen X Y lin
= nil fab lin

: range z z -- {r* -- r* z}
= range
  (LO HI)~ (LO <= HI) (
       nil
    || LO (HI - 1) range [HI] snoc
  )

= main main
