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

  val enter_let : t
  val leave_let : t

  val enter_for : t
  val leave_for : t

  val enter_while : t
  val leave_while : t

  val enter_apply : t
  val leave_apply : t

end

module DefaultIterator = struct

  let enter_fun ?info args exp = exp
  let leave_fun ?info args exp = exp

  let enter_match ?info args exp = exp
  let leave_match ?info args exp = exp

  let enter_let ?info args exp = exp
  let leave_let ?info args exp = exp

  let enter_for ?info args exp = exp
  let leave_for ?info args exp = exp

  let enter_while ?info args exp = exp
  let leave_while ?info args exp = exp

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

  and wrap_case ?fname args m c =
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
            None -> Some (Format.asprintf "case: @[<h>%a@]" Pprintast.pattern c.pc_lhs)
          | Some s ->
            Some (Format.asprintf "%s, case: @[<h>%a@]" s Pprintast.pattern c.pc_lhs) in
        add_debug_infos ?info args c'.pc_rhs enter_match leave_match}

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
    | Pexp_apply (f, _) ->
      let f_str =
        match f.pexp_desc with
        | Pexp_ident ({txt = Lident id; loc}) -> Some id
        | _ -> None in
      add_debug_infos ?info:f_str args e enter_apply leave_apply
    | Pexp_while _ ->
      add_debug_infos ?info:fname args e enter_while leave_while
    | Pexp_for _ ->
      add_debug_infos ?info:fname args e enter_for leave_for
    | _ -> default_mapper.expr m e

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
