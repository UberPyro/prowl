= pred (- 1)

= fib
  dup (> 1)
    pred (fib && pred) fib (+)
  elim

= main 5 fib
