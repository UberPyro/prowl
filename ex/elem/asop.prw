let eith-map {f} = (as (;x) -> (;x f))? -> 
let (as+) = eith-map -> 
(;4) (as+ y -> y * 2)
as (;w) -> w
