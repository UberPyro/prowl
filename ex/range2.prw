= pred (- 1)
= le (<=)

: range z z -- { -- z}
= range (le && id) dup (pred range ** unit) lin

= main main
