/* dequoting? */
let eith-map f = (as (;x) -> (;x f))? -> 

let (as+) = eith-map -> 


(;4) (as+ y -> y * 2)

as (;w) -> w





/* reminder: fix closures */
/* 
(;4) {as y -> y * 2} eith-map
as (;w) -> w
*/

/* need dequoting */
/* test HOFs */
