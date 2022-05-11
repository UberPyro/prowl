let nfib n m = 
  let nfib-step a = a m take sum (a >-) ->
  [0, 1] nfib-step{n} hd -> 

to-int % to-int % nfib
