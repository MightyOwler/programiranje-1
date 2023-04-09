let dot_prod (x, y, z) (a, b,c) = x *. a +. y *. b +. z *. c


let fix_second f el = (fun x -> f x el)