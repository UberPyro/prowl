let fib-step m n = m + n & m
and fib n = 1 0 fib-step{n} _ ->
to-int fib
