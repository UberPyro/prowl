let fac-step n a = n > 0 && (n - 1 & a * n)
and fac = 1 fac-step* _2 ->
to-int fac
