open Ppx_debug

module MyIterator = MakeIterator(PrintIterator)


let () =
  register "debug" (fun args -> MyIterator.wrap_debug args)
