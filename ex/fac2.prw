let rec fac = 
  (= 0) 1
  | as n -> n * (n - 1) fac in

fac
