let nfib n m = 
  let nfib-step = ^ m take sum (>-) ->
  [0, 1] nfib-step{n} hd -> 

to-int % to-int % nfib
