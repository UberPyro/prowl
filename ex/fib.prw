let n fib
  = (n == 0) 0
  | (n == 1) 1
  | (n - 1) fib + (n - 2) fib
