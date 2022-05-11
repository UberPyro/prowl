let rules = [
  (3, "fizz"), 
  (5, "buzz"), 
  (7, "bizz")
]

and fizzbuzz = 
  (1 ..) as+ n -> 
  rules <?> (n % div) <&> snd
  & fold (null n to-str)? -> 

to-int fizzbuzz <#> put
