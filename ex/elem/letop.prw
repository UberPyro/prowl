let eith-map {f} = (as {(;x)} -> (;x f))? -> 
let (let+) = eith-map -> 
(let+ y = (;4) -> y * 2)
as (;w) -> w
