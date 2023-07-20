open! Batteries
open! Either
open Ucommon

module Make (L : UNIFIABLE) (R : UNIFIABLE) = struct
  
  type t = t_unif uref
  and t_unif =  
    | Left of L.t
    | Right of R.t

  type memo = unit

  let memo () = ()
  let refresh_memo () = 
    L.refresh_memo ();
    R.refresh_memo ()

  let uleft x = uref @@ Left x
  let uright x = uref @@ Right x
  let uleft_delayed f () = uref @@ Left (f ())
  let uright_delayed f () = uref @@ Right (f ())

  let unify = 
    unite ~sel:begin curry @@ function
      | Left x, Left y -> 
        L.unify x y;
        Left x
      | Right x, Right y -> 
        R.unify x y;
        Right x
      | _ -> 
        UnifError "Cannot unify differently sorted terms"
        |> raise
    end

  let occurs i = uget %> function
    | Left x -> L.occurs i x
    | Right x -> R.occurs i x
  
  let generalize () t = match uget t with
    | Left x -> uref @@ Left (L.generalize (L.memo ()) x)
    | Right x -> uref @@ Right (R.generalize (R.memo ()) x)
  
  let pretty out = uget %> function
    | Left x -> L.pretty out x
    | Right x -> R.pretty out x
  
end
