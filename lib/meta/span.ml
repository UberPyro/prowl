open! Batteries

include Util.Lattice.TSeg

let tbl : (string * int, int * int) Hashtbl.t = Hashtbl.create 32

let make loc = 
  Tuple2.mapn begin fun (p : Lexing.position) -> 
    Hashtbl.add tbl
      (p.pos_fname, p.pos_cnum)
      (p.pos_lnum, p.pos_cnum - p.pos_bol)
  end loc |> ignore;
  Map.add
    (fst loc).pos_fname
    (ISet.add_range (fst loc).pos_cnum (snd loc).pos_cnum ISet.empty)
    Map.empty
