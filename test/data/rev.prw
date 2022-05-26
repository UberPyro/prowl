let (>-) {t} {h} = (;t,h) -> 
let rev-step = as (t >- h) a -> t (a >- h) -> 
let rev = [] rev-step*+ _2 -> 

[1, 3, 5, 7, 9] rev

as t1 >- h1 -> h1
t1 as t2 >- h2 -> h2
t2 as t3 >- h3 -> h3
t3 as t4 >- h4 -> h4
t4 as t5 >- h5 -> h5
