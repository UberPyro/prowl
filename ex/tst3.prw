= pred (- 1)

= h pred



: fib z -- z
= fib
  dup (> 1)
    pred (fib && pred) fib (+)
  elim


= main 5 fib
