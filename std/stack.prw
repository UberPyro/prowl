mod

  def (>-) t h = (;t,h)

  local def rev-step (t >- h) a = t (a >- h)
  def rev = [] rev-step*+ _2

end
