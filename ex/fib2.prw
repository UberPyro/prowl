= pred (- 1)

: fib z -- z
= fib
  dup (> 1)
    pred (fib && pred) fib (+)
  elim

= main 5 fib
