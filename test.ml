let f x = x + x

let g = function
    Some _ -> f 1
  | None -> f 2

let h x y z = ()

let x = 50

let _ =
  ignore (f x);
  ignore (g (Some 2))
