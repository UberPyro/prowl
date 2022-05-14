let s x = x + 1 ->
let f y {g} = y g ->
4 {s} f
