
let f x = x + x

let g = function
    Some _ -> f 1
  | None -> f 2

let h x y z = ()

let x = 50

let z = ()

let it min max =
  for i=min to max do
    Printf.eprintf "%d " i
  done;
  Printf.eprintf "\n%!"

let _ =
  for i=0 to 9 do
    Printf.eprintf "%d " i
  done;
  it 0 10;
  ignore (f x);
  ignore (g (Some 2))
