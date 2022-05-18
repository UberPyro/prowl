mod
  def impl m = mod def s x = x + 1 end
  def add-two <q : int> (y : int) = y s s
  
  def four = 0 add-two add-two
end.four
