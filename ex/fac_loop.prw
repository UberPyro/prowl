= pred (- 1)

= fac-step dup (> 0) pred dup ((*) ** id)

= fac 1 swap (fac-step @) zap

= main 5 fac
