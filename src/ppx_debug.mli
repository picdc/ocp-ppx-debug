module type DebugIterator = sig
  val name : string

  val enter_fun : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_fun : ?info:string -> Parsetree.expression -> Parsetree.expression

  val enter_match : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_match : ?info:string -> Parsetree.expression -> Parsetree.expression

  val enter_let : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_let : ?info:string -> Parsetree.expression -> Parsetree.expression

  val enter_for : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_for : ?info:string -> Parsetree.expression -> Parsetree.expression

  val enter_while : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_while : ?info:string -> Parsetree.expression -> Parsetree.expression

  val enter_apply : ?info:string -> Parsetree.expression -> Parsetree.expression
  val leave_apply : ?info:string -> Parsetree.expression -> Parsetree.expression
end

module DefaultIterator : DebugIterator
module PrintIterator : DebugIterator

module MakeIterator :
  functor (Iter : DebugIterator) ->
   sig
     val wrap_debug : Ast_mapper.mapper
   end

(**
   This function allows to inject the wrapping code to display the debug
   information.
   @info:string: the parent function name
   @Parsetree.expression: the Parsetree.expression to wrap
   @string: message to prefix
  *)
val print :
  ?info:string -> Parsetree.expression -> string -> Parsetree.expression

val add_debug_infos :
  ?info:string ->
  Parsetree.expression ->
  (?info:string -> Parsetree.expression -> Parsetree.expression) ->
  (?info:string -> Parsetree.expression -> Parsetree.expression) ->
  Parsetree.expression
