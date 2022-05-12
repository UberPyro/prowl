let fac-step a m = a * m & m - 1
and fac n = 1 n fac-step{n} _ -> 
to-int fac
