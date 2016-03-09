open Parsetree
open Asttypes
open Longident
open Ast_mapper
open Ast_helper

module TraceIterator = struct
  include Ppx_debug.DefaultIterator
  open Ppx_debug
  let on_debug exp =
    let loc = exp.pexp_loc in
    Exp.ifthenelse ~loc
      (Exp.apply ~loc
         (Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "=") })
         ["", (Exp.apply ~loc
                 (Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "!") })
                 ["", (Exp.ident ~loc
                         { loc; txt = Ldot (Lident "Clflags", "dump_lambda_infos") })]);
          "", (Exp.construct ~loc { loc; txt = Lident "true" } None)])
      exp
      None

  let get_curr loc =
    Exp.apply ~loc 
      (Exp.ident ~loc { loc; txt = Ldot (Lident "Debug", "get") })
      ["", Exp.ident ~loc { loc; txt = Lident "()" }]

  let curr_id loc = Exp.ident ~loc { loc; txt = Lident "*curr-id*" }

  let try_id loc = Exp.ident ~loc { loc; txt = Lident "*try-id*" }

  let incr loc =
    Exp.apply ~loc
      (Exp.ident ~loc { loc; txt = Ldot (Lident "Debug", "incr") })
      ["", (Exp.construct ~loc { loc; txt = Lident "()" } None)]

  let decr loc =
    Exp.apply ~loc
      (Exp.ident ~loc { loc; txt = Ldot (Lident "Debug", "decr") })
      ["", (Exp.construct ~loc { loc; txt = Lident "()" } None)]

  let gen_curr_id expr =
    Exp.let_ ~loc:expr.pexp_loc Nonrecursive
      [Vb.mk ~loc:expr.pexp_loc
         (Pat.var ~loc:expr.pexp_loc { loc = expr.pexp_loc; txt = "*curr-id*" })
         (incr expr.pexp_loc)]
      expr

  let gen_try_id expr = 
    Exp.let_ ~loc:expr.pexp_loc Nonrecursive
      [Vb.mk ~loc:expr.pexp_loc
         (Pat.var ~loc:expr.pexp_loc { loc = expr.pexp_loc; txt = "*try-id*" })
         (incr expr.pexp_loc)]
      expr
  
  let print ?info exp prefix id =
    let loc = exp.pexp_loc in
    let msg = match info with None -> prefix | Some f -> prefix ^ " " ^ f in
    Exp.sequence
      (Exp.apply ~loc
         (Exp.ident ~loc { loc; txt = Ldot (Lident "Format", "eprintf"); })
         ["", Exp.constant ~loc
            (
              (Const_string (Printf.sprintf "[%%d]%s %%s\n%!" msg, None))
            );
          "", id loc;
          "", Exp.ident ~loc
            ({ loc; txt = Ldot (Lident "Pervasives", "__LOC__")})
         ])
      exp
  
  let enter_fun ?info _ exp =
    gen_curr_id @@
    print ?info exp "[->]Entering fun" curr_id
  let leave_fun ?info _ exp =
    print ?info exp "[<-] Leaving fun" curr_id

  let enter_match ?info _ exp =
    gen_curr_id @@
    print ?info exp "[->]Entering match" curr_id
  let leave_match ?info _ exp =
    print ?info exp "[<-] Leaving match" curr_id

  let enter_case ?info _ exp =
    gen_curr_id @@
    print ?info exp "[->]Entering case" curr_id
  let leave_case ?info _ exp =
    print ?info exp "[<-] Leaving case" curr_id

  let enter_try ?info _ exp =
    gen_curr_id @@
    gen_try_id @@
    print ?info exp "[->]Entering try" curr_id
  let leave_try ?info _ exp =
    print ?info exp "[<-] Leaving try" curr_id

  let enter_try_body ?info _ exp =
    print ?info exp "[->]Entering try body" try_id
  let leave_try_body ?info _ exp =
    print ?info exp "[<-] Leaving try body" try_id  

  let enter_try_handler ?info _ exp =
    let exp = print ?info exp "[->]Entering try handler" curr_id in
    print ?info exp "[<-] Leaving try body (caught exception)" try_id
  let leave_try_handler ?info _ exp =
    print ?info exp "[<-] Leaving try handler" curr_id
      
  let enter_let ?info _ exp =
    gen_curr_id @@
    print ?info exp "[->]Entering let" curr_id
  let leave_let ?info _ exp =
    print ?info exp "[<-] Leaving let" curr_id

  let enter_for ?info _ exp =
    gen_curr_id @@
    print ?info exp "[->]Entering for" curr_id
  let leave_for ?info _ exp =
    print ?info exp "[->] Leaving for" curr_id
      
  let enter_apply ?info args exp = 
    gen_curr_id @@
    print ?info exp "[->]Calling fun" curr_id
  let leave_apply ?info args exp =
    print ?info exp "[<-]Finished fun" curr_id

end

module TraceDebug = Ppx_debug.MakeIterator(TraceIterator)

let _ =
  register "trace_debug" TraceDebug.wrap_debug
