= fac ex N -> 
  (N > 1) exch (
       N * (N - 1) fac
    || 0
  )

= main 5 fac
