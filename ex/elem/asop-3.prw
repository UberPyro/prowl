/* let eith-map {f} = (as (;x) -> (;x f))? -> */
let eith-map (;x) {f} = (;x f) ->
let double = (* 2) ->
(;4) {double} eith-map
as (;w) -> w
