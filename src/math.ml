
let distance2d (x , y) (z , w) =
  sqrt ((x-.z) *. (x-.z) +. (y -. w) *. (y -. w))

let length2d (x , y) = distance2d (x , y) (0.0 , 0.0)

let perpendicular (x , y) =
  (-.y , x)

let scale_to s (x , y) =
  let l = length2d (x , y) in
  (s /. l *. x , s /. l *. y)

let add2d (x , y) (z , w) = (x +. z , y +. w)
                          
let scale2d s (x , y) = (s*.x , s*.y)
