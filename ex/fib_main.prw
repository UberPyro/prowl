= main
  dup (> 2)
    (- 1) (main && (- 1) main) (+)
  elim
