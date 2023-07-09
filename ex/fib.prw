= fib
  dup (> 2)
    (- 1) (fib && (- 1) fib) (+)
  elim

= main fib
