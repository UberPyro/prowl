main := "Hello, World!"

fac := match
  : 0 => 1
  : N => N * (N-1) fac
end

fib := match
  : 0 => 0 : 1 => 1
  : N => (N-1) fib + (N-2) fib
end

data A list = (eps | A list A) @pro

map := @pro F => F map ** F cat @pro
