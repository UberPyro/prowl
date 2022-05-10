let fac-step a n = a - 1 & n * a
and fac n = 1 n fac-step{n} _ -> 
to-int fac
