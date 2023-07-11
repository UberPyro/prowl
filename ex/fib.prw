= main
  dup (> 1)
    (- 1) (main && (- 1)) main (+)
  elim
