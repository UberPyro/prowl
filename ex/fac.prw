= pred (- 1)
= succ (+ 1)

= fac 
  dup (== 0) (dup pred fac (*) || succ)
