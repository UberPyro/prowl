let prog = "fmod DISTACK is
sort V S C FN .

op int : -> V .
op str : -> V .
op quo : FN -> V [ctor] .
op lst : FN -> V [ctor] .

op val : V -> S [ctor] .
op unit : -> S .
op also : S S -> S [ctor assoc id:unit] .

op stack : S -> C [ctor] .
op void : -> C .
op else : C C -> C [ctor assoc id:void] .

op fn : C C -> FN .
endfm"
