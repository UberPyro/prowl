5 mod
  def f = [
    as 0 -> "hi";
    as n -> n - 1 & f
  ]
end.f
