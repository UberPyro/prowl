
= rot ex X Y Z -> Y Z X
= trip ex X -> X X X
= same3 trip~
= del3 ex X X X -> nop

= f swap rot swap
= g trip same3
= h [f] [g] (>>)
= i 0 | 0

= main main
