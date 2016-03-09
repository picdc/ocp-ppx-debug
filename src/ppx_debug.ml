open Parsetree
open Asttypes
open Longident
open Ast_helper

type t = ?info:string -> string list -> expression -> expression

let print exp format args =
  let loc = exp.pexp_loc in
  let args = List.map (fun e -> ("", e)) args in
  let print =
    Exp.sequence ~loc
      (Exp.apply ~loc
         (Exp.ident ~loc { loc; txt = Ldot (Lident "Format", "eprintf")})
         (("", Exp.constant ~loc (Const_string (format, None))) :: args))
      exp in
  print

let add_debug_infos ?info args exp enter leave =
  let loc = exp.pexp_loc in
  let exp' =
    Exp.let_ ~loc Nonrecursive
      [Vb.mk ~loc
          (Pat.var ~loc {loc; txt = "expr"})
          exp] @@
      leave ?info args @@ Exp.ident ~loc {loc; txt = Lident "expr"}
  in
  enter ?info args exp'

module type DebugIterator = sig

  val enter_fun : t
  val leave_fun : t

  val enter_match : t
  val leave_match : t

  val enter_case : t
  val leave_case : t

  val enter_let : t
  val leave_let : t

  val enter_for : t
  val leave_for : t

  val enter_while : t
  val leave_while : t

  val enter_try : t
  val leave_try : t

  val enter_try_body : t
  val leave_try_body : t

  val enter_try_handler : t
  val leave_try_handler : t

  val enter_apply : t
  val leave_apply : t

end

module DefaultIterator = struct

  let enter_fun ?info args exp = exp
  let leave_fun ?info args exp = exp

  let enter_match ?info args exp = exp
  let leave_match ?info args exp = exp

  let enter_case ?info args exp = exp
  let leave_case ?info args exp = exp

  let enter_let ?info args exp = exp
  let leave_let ?info args exp = exp

  let enter_for ?info args exp = exp
  let leave_for ?info args exp = exp

  let enter_while ?info args exp = exp
  let leave_while ?info args exp = exp

  let enter_try ?info args exp = exp
  let leave_try ?info args exp = exp

  let enter_try_body ?info args exp = exp
  let leave_try_body ?info args exp = exp

  let enter_try_handler ?info args exp = exp
  let leave_try_handler ?info args exp = exp

  let enter_apply ?info args exp = exp
  let leave_apply ?info args exp = exp

end

module PrintIterator = struct

  let extract = function None -> "" | Some f -> f

  let enter_fun ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering fun %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let leave_fun ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving fun %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_match ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering match %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_match ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving match %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_case ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering case %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_case ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving case %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_let ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering let %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let leave_let ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving let %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_for ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering for %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_for ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving for %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_while ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering while %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_while ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving while %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_try ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering try %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let leave_try ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving try %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_try_body ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering try body %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_try_body ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving try body %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_try_handler ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Entering try handler %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_try_handler ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,Leaving try handler %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

  let enter_apply ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@,@[<v 2>Calling fun %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]
  let leave_apply ?info args exp =
    let info = extract info in
    let format = Printf.sprintf "@]@,End of call of %s %%s" info in
    let loc = exp.pexp_loc in
    print exp format [Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives", "__LOC__")}]

end

(* module DumpIterator = struct *)
(* TODO: write the output in a file *)
(* end  *)

module MakeIterator (Iter : DebugIterator) : sig

  val wrap_debug : string list -> Ast_mapper.mapper

end = struct
  open Ast_mapper
  open Iter

  let rec wrap_sequences ?fname args m e =
    match e.pexp_desc with
    | Pexp_sequence (e1, e2) ->
      let e1' = default_mapper.expr m e1 in
      let e2' = default_mapper.expr m e2 in
      Exp.sequence e1' e2'
    | _ -> default_mapper.expr m e

  and wrap_case_generic ?fname args m c enter_case leave_case =
    let c' =
      { pc_lhs = default_mapper.pat m c.pc_lhs;
        pc_guard =
          (match c.pc_guard with
             Some g -> Some (wrap_expr args m g)
           | None -> None);
        pc_rhs = wrap_expr args m c.pc_rhs } in
    { c' with
      pc_rhs =
        let info = match fname with

            None -> Some (Format.asprintf "pattern: @[<h>%a@]" Pprintast.pattern c.pc_lhs)
          | Some s ->
            Some (Format.asprintf "%s, pattern: @[<h>%a@]" s Pprintast.pattern c.pc_lhs) in
        add_debug_infos ?info args c'.pc_rhs enter_case leave_case}

  and wrap_case ?fname args m c =
    wrap_case_generic ?fname args m c enter_case leave_case

  and wrap_expr ?fname args m e =
    match e.pexp_desc with
      Pexp_function
        [{pc_rhs = { pexp_desc = Pexp_function _ | Pexp_fun _ } as e'} as c'] ->
      Exp.function_ [{c' with pc_rhs = wrap_expr args m e' ?fname}]
    | Pexp_fun (l, eopt, p, ({pexp_desc = Pexp_function _ | Pexp_fun _ } as e'))
      ->
      Exp.fun_ l eopt p (wrap_expr ?fname args m e')
    | Pexp_fun (l, eopt, p, e') ->
      Exp.fun_ l eopt p
        (add_debug_infos ?info:fname args e' enter_fun leave_fun)
    | Pexp_apply (f, es) ->
      let f_str =
        match f.pexp_desc with
        | Pexp_ident ({txt = Lident id; loc}) -> Some id
        | _ -> None in
      let e =
        Exp.apply ~loc:e.pexp_loc
          (wrap_expr ?fname args m f)
          (List.map (fun (l, e) -> l, wrap_expr ?fname args m e) es) in
      add_debug_infos ?info:f_str args e enter_apply leave_apply
    | Pexp_while _ ->
      add_debug_infos ?info:fname args e enter_while leave_while
    | Pexp_for _ ->
      add_debug_infos ?info:fname args e enter_for leave_for
    | Pexp_match (e', cs) ->
      let e =
        Exp.match_ ~loc:e.pexp_loc
          (wrap_expr ?fname args m e')
          (List.map (wrap_case ?fname args m) cs) in
      add_debug_infos ?info:fname args e enter_match leave_match
    | Pexp_try (e', cs) ->
      let e =
        Exp.try_ ~loc:e.pexp_loc
          (wrap_try_body ?fname args m e')
          (List.map (wrap_try_handler ?fname args m) cs) in
      add_debug_infos ?info:fname args e enter_try leave_try
    | Pexp_let (rf, vbs, e') ->
      Exp.let_ ~loc:e.pexp_loc rf
        (List.map (wrap_value_binding ?fname args m) vbs)
        (wrap_expr ?fname args m e)
    | _ ->  default_mapper.expr m e

  and wrap_try_body ?fname args m e =
    add_debug_infos ?info:fname args
      (wrap_expr ?fname args m e)
      enter_try_body
      leave_try_body

  and wrap_try_handler ?fname args m e =
   wrap_case_generic ?fname args m e enter_try_handler leave_try_handler

  and wrap_value_binding ?fname args m vb =
    let fname =
      match vb.pvb_pat.ppat_desc with
        Ppat_var { txt = l } -> Some l
      | _ -> None in
    { vb with pvb_expr = wrap_expr args m vb.pvb_expr ?fname }

  and wrap_debug args =
    { default_mapper with
      expr = wrap_expr args;
      case = wrap_case args;
      value_binding = wrap_value_binding args;
    }

end

let register = Ast_mapper.register
