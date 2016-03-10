open Ppx_debug


let extract = function None -> "" | Some f -> f

module MyPrinter = struct
  include PrintIterator

  let enter_fun ?info args exp =
    let format = Printf.sprintf "[->] %s %%s\n" (extract info) in
    print exp format [[%expr Pervasives.__LOC__]]

end
module MyIterator = MakeIterator(MyPrinter)


let () =
  register "debug" (fun args -> MyIterator.wrap_debug args)
