= fac ex N -> 
  (N < 2) (
       N * (N - 1) fac
    || 0
  )

= main 5 fac
