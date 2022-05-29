let (>>) f g x = x |> f |> g
let (>>>) f g x = let y, z = f x in g y z
let (>>>>) f g x = let y, z, w = f x in g y z w

let (||>) (x, y) f = f x y
let (|||>) (x, y, z) f = f x y z

let (<|) f x = f x
let (<||) f (x, y) = f x y
let (<|||) f (x, y, z) = f x y z

let (<&>) d f = List.map f d
let (&>) d v = d <&> fun _ -> v
let (>>=) m f = m <&> f |> List.flatten

let (let*) = (>>=)
let (let+) = (<&>)

let (<@>) d f = let* x = d in let+ g = f in g x
let (@>) d v = d <&> fun _ -> v
let (>>:) = (@>)
let (let@) = (<@>)

module Either = struct

  type ('a, 'b) t = L of 'a | R of 'b

  let (<&>) d f = match d with
    | R r -> R (f r)
    | e -> e
  
  let (>>=) m f = match m with
    | R r -> f r
    | e -> e
  
  let either e f g = match e with
    | L l -> f l
    | R r -> g r
  
  let bimap e f g = match e with
    | L l -> L (f l)
    | R r -> R (g r)
  
  let (let*) = (>>=)
  let (let+) = (<&>)

end

type ('a, 'b) either = ('a, 'b) Either.t

let dum = Lexing.(dummy_pos, dummy_pos)

let rec apply_for n f x = 
  if n > 0 then apply_for (n-1) f (f x)
  else x

let rec apply_while c f x = 
  if c x then apply_while c f (f x)
  else x

let rec bind_while f x = 
  match f x with
  | Some y -> bind_while f y
  | None -> x
