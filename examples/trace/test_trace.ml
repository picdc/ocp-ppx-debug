let f x y = ((x + y) : int)

let rec range i = if i <= 0 then [] else
    let rem = range (i-1) in
    if i = 5 then failwith "Ok" else i :: rem

let mk l = List.fold_left f 0 l

let _ = mk @@ try range 10 with _ -> [ 1; 4 ;5 ]
