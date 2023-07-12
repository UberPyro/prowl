= pred (- 1)
= le (<=)

: range z z -- {r* -- r* z}
= range (le && id) dup (pred range ** unit) lin

= main main
