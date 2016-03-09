
module Make () : sig
  val incr: unit -> int
  val decr: unit -> int
  val get: unit -> int
  val reset: unit -> unit
end = struct
  let curr = ref 0
  let incr () = incr curr; !curr 
  let decr () =
    let c = !curr in decr curr; c
  let reset () = curr := 0
  let get () = !curr
end

module M = Make ()
include M

