let (>-) {t} {h} = (;t,h) ->
let rev-step = as (t >- h) a -> t (a >- h) ->
[1, 2] [] rev-step{2}
