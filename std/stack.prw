mod

  def (>-) t h = (;t,h)
  def empty = []

  local def rev-step (t >- h) a = t (a >- h)
  def rev = [] rev-step*+ _2

  def map-rev {f} = 
    let map-step (t >- h) a = t (a >- h f) ->
    [] map-step*+ _2
  
  def map = map-rev rev
  def (<&>) {s} {f} = s {f} map
  def (as+) = map

  def cat-rev = % rev-step*+ _2
  def cat = rev % rev-step*+ _2
  def (++) {s1} {s2} = s1 s2 cat

  def bind-rev {f} = 
    let bind-step (t >- h) a = t (a ++ h f) -> 
    [] bind-step*+ _2
  
  def bind = bind-rev rev
  def (>>=) {s} {f} = s {f} map
  def (as*) {f} = {f} map

  def fold-left i {r} = 
    let fold-left-step (t >- h) a = t (a h r) -> 
    i fold-left-step*+ _2
  
  def monoid m = mod
    def fold = m.empty {m.cat} fold-left
  end

  def filter-rev s {f} = 
    let filter-step (t >- h) a = t a (h f (>- h))? -> 
    [] filter-step*+ _2
  
  def filter = filter-rev rev
  def (<?>) {s} {f} = s {f} filter
  def (as?) = filter

end
