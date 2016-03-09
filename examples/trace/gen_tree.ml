
exception Ill_formed_output
  
let open_list id info fmt =
  Printf.fprintf fmt "<li>%s\n<ul id=\"%d\">\n" info id

let close_list info fmt =
  Printf.fprintf fmt "</li>\n%s</ul>\n" info

let previous : int list ref = ref []

let push_id id info fmt =
  previous := id :: !previous;
  open_list id info fmt

let rec close_previous id info oc =
  match !previous with
    [] -> raise Ill_formed_output
  | id' :: rem ->
    close_list (if id' = id then info else "Possible exception caught") oc;
    previous := rem;
    if id <> id' then close_previous id info oc 

(* Hadoc parsing of [id][->] ... *)
let parse_line ic fmt =
  let l = input_line ic in
  if l.[0] = '[' then
    let id_end = String.index l ']' in
    let id = int_of_string (String.sub l 1 (id_end - 1)) in
    let rem = String.sub l (id_end + 1) (String.length l - id_end - 1) in
    if rem.[0] = '[' then
      let dir_end = String.index rem ']' in
      let dir = (String.sub rem 1 (dir_end - 1)) in
      if dir = "->" then
        push_id
          id
          (String.sub rem (dir_end + 1) (String.length rem - dir_end - 1))
          fmt
      else if dir = "<-" then
        close_previous
          id
          (String.sub rem (dir_end + 1) (String.length rem - dir_end - 1))
          fmt
      else raise Ill_formed_output
    else raise Ill_formed_output
  else raise Ill_formed_output

let read_file ic fmt =
  output_string fmt "<html><body>\n";
  try
    while true do parse_line ic fmt done
  with End_of_file ->
    output_string fmt "</body></html>"

let () =
  let in_file = Sys.argv.(1) in
  let out_file = Sys.argv.(2) in
  let ic = open_in in_file in
  let oc = open_out out_file in
  try
    read_file ic oc;
    close_in ic;
    close_out oc
  with exn ->
    close_in ic;
    close_out oc;
    raise exn
    
