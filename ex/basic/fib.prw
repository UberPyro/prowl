let fib-step m n = m + n & m
and fib n = 1 0 fib-step{n-1} _ | 0 ->
to-int fib
