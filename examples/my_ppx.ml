open Ppx_debug

module MyIterator = MakeIterator(PrintIterator)

let _ =
  Ast_mapper.register "debug" (fun _ -> MyIterator.wrap_debug)
