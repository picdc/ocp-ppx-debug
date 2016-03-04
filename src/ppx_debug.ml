open Parsetree
open Asttypes
open Longident
open Ast_helper

type t = ?info:string -> string list -> expression -> expression

let print ?info exp prefix =
  let loc = exp.pexp_loc in
  let msg = match info with None -> prefix | Some f -> prefix ^ " " ^ f in
  let print =
    Exp.apply ~loc
      (Exp.ident ~loc { loc; txt = Ldot (Lident "Format", "eprintf"); })
      ["", Exp.constant ~loc
        (
          (Const_string (Printf.sprintf "%s %%s\n%!" msg, None))
        );
       "", Exp.ident ~loc
         ({ loc; txt = Ldot (Lident "Pervasives", "__LOC__")})
      ] in
  print

let add_debug_infos ?info args exp enter leave =
  let loc = exp.pexp_loc in
  let enter = enter ?info args exp in
  let leave = leave ?info args exp in
  let exp' =
    Exp.let_ ~loc Nonrecursive
      [Vb.mk ~loc
          (Pat.var ~loc {loc; txt = "expr"})
          exp] @@
      Exp.sequence ~loc leave @@ Exp.ident {loc; txt = Lident "expr"}
  in
  Exp.sequence ~loc enter exp'

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

  let dummy exp =
    let loc = { txt = Lident "()"; loc = exp.pexp_loc} in
    { exp with pexp_desc = Pexp_construct (loc, None) }

  let enter_fun ?info args exp = dummy exp
  let leave_fun ?info args exp = dummy exp

  let enter_match ?info args exp = dummy exp
  let leave_match ?info args exp = dummy exp

  let enter_let ?info args exp = dummy exp
  let leave_let ?info args exp = dummy exp

  let enter_for ?info args exp = dummy exp
  let leave_for ?info args exp = dummy exp

  let enter_while ?info args exp = dummy exp
  let leave_while ?info args exp = dummy exp

  let enter_apply ?info args exp = dummy exp
  let leave_apply ?info args exp = dummy exp

end

module PrintIterator = struct

  let enter_fun ?info args exp =
    print ?info exp "Entering fun"
  let leave_fun ?info args exp =
    print ?info exp " Leaving fun"

  let enter_match ?info args exp =
    print ?info exp "Entering match"
  let leave_match ?info args exp =
    print ?info exp " Leaving match"

  let enter_let ?info args exp =
    print ?info exp "Entering let"
  let leave_let ?info args exp =
    print ?info exp " Leaving let"

  let enter_for ?info args exp =
    print ?info exp "Entering for"
  let leave_for ?info args exp =
    print ?info exp " Leaving for"

  let enter_while ?info args exp =
    print ?info exp "Entering while"
  let leave_while ?info args exp =
    print ?info exp " Leaving while"


  let enter_apply ?info args exp =
    print ?info exp "Calling fun"
  let leave_apply ?info args exp =
    print ?info exp "End of call of"

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
            None -> Some (Format.asprintf "case: %a" Pprintast.pattern c.pc_lhs)
          | Some s ->
            Some (Format.asprintf "%s, case: %a" s Pprintast.pattern c.pc_lhs) in
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
    | _ ->  default_mapper.expr m e

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
