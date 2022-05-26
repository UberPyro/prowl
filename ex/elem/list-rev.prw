let m = mod

  def (>-) {t} {h} = (;t,h)
  def rev-step = as (t >- h) a -> t (a >- h)
  def rev = [] rev-step*+ _2

end -> 

/* [1, 3, 5, 7, 9] m.rev */
[1, 3, 5] [] m.rev-step{2}
