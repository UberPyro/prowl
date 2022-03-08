let (>>) f g = fun x -> x |> f |> g
let (<&>) d f = List.map f d
