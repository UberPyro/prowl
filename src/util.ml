let (>>) f g x = x |> f |> g
let (>>>) f g x = let y, z = f x in g y z
let (>>>>) f g x = let y, z, w = f x in g y z w

let (||>) (x, y) f = f x y
let (|||>) (x, y, z) f = f x y z

let (<|) f x = f x
let (<||) f (x, y) = f x y
let (<|||) f (x, y, z) = f x y z