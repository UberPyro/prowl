let m = mod

  def (>-) {t} {h} = (;t,h)
  local def rev-step (t >- h) a = t (a >- h)
  def rev = [] rev-step*+ _2

end -> 

[1, 3, 5, 7, 9] m.rev
