type t =
  ?info:string -> string list -> Parsetree.expression -> Parsetree.expression

module type DebugIterator = sig

  val enter_fun : t
  val leave_fun : t

  val enter_match : t
  val leave_match : t

  val enter_let : t
  val leave_let : t

  val enter_for : t
  val leave_for : t

  val enter_while : t
  val leave_while : t

  val enter_apply : t
  val leave_apply : t
end

module DefaultIterator : DebugIterator
module PrintIterator : DebugIterator

module MakeIterator :
  functor (Iter : DebugIterator) ->
   sig
     val wrap_debug : string list -> Ast_mapper.mapper
   end

(**
   This function allows to inject the wrapping code to display the debug
   information.
   @info:string: the parent function name
   @Parsetree.expression: the Parsetree.expression to wrap
   @string: message to prefix
  *)
val print :
  Parsetree.expression ->
  string ->
  Parsetree.expression list ->
  Parsetree.expression

val add_debug_infos :
  ?info:string ->
  string list ->
  Parsetree.expression ->
  t ->
  t ->
  Parsetree.expression

val register: string -> (string list -> Ast_mapper.mapper) -> unit
