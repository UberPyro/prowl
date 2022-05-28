let (>-) {t} {h} = (;t,h) -> 
let rev-step (t >- h) a = t (a >- h) -> 
let rev = [] rev-step*+ _2 -> 

[1, 3, 5, 7, 9] rev
as _ >- h -> h
