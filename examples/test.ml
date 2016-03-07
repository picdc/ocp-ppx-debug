let x = 50

let f x = x + x

let g = function
    Some _ -> f 1
  | None -> f 2

let h x y z = ()


let z = ()

let it min max =
  for i=min to max do
    let res = i + (f 2) in
    Printf.printf "%d " res
  done;
  Printf.printf "\n%!"

let itwhile cond =
  let i = ref 0 in
  while !i < cond do
    let res = !i + (f 2) in
    incr i;
    Printf.printf "%d " res
  done;
  Printf.printf "\n%!"

let _ =
  let maxx = 9 in

  for i=0 to maxx do
    Printf.printf "%d \n" i
  done;
  itwhile 10;
  it 0 10;
  (* while true do () done; *)
  ignore (f x);
  ignore (g (Some 2));
  let toto x = x in
  toto 2
