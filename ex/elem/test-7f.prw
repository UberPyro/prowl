let (>-) {t} {h} = (;t,h) -> 
let rev-step = as (t >- h) a -> t (a >- h) -> 
let rev = [] rev-step*+ _2 -> 

[1, 3, 5, 7, 9] rev
