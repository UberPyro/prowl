let fac-step a n = a * n & n - 1
and fac n = 1 n fac-step{n} _ -> 
to-int fac
