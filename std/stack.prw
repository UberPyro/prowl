mod

  def (>-) t h = (;t,h)

  local def rev-step (t >- h) a = t (a >- h)
  def rev = [] rev-step*+ _2

  def map-rev {f} = 
    let map-step (t >- h) a = t (a >- h f) ->
    [] map-step*+ _2
  
  def map = map-rev rev
  def (<&>) {s} {f} = s {f} map
  def (as+) {f} = {f} map

end
