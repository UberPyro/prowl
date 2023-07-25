= pred (- 1)
= succ (+ 1)

= h succ pred
/*
= f pred pred
= g (+ 1) (+ 1)
*/
/* 
= g pred && pred
= h 0 ** 0
= i pred ** pred
= j pred ** succ
*/ 
/* 
: fib z -- z
= fib
  dup (> 1)
    pred (fib && pred) fib (+)
  elim

= main 5 fib
*/
= main main
