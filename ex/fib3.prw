= pred (- 1)

: fib z -- z
= fib
  dup (> 1)
    pred dup fib swap pred fib (+)
  elim

= main 5 fib
