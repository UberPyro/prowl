let fib-step n m = m & n + m ->
let fib n = 0 1 (fib-step{n - 1} %)? _ ->
to-int fib
