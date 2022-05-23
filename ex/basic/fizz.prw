/* warning: this abuses dynamic typing in an unintended way :) */

let div n m = n / m * m == n -> 
let fizz = [
  15 div "fizzbuzz";
  3 div "fizz"; 
  5 div "buzz"
]? -> 
let rep = (as n m {f} -> (n <= m) n f (n + 1) m {f})* -> 
to-int 1 % {fizz} rep _ _ _
