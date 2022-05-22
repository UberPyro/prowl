mod

  def (>-) t h = (;t,h)

  local def rev-step (t >- h) a = t (a >- h)
  def rev = [] rev-step*+ _2

  def map-rev {f} = 
    let map-step (t >- h) a = t (a >- h f) ->
    [] map-step*+ _2
  
  def map = map-rev rev

end as open ->

[1, 2, 3] rev
