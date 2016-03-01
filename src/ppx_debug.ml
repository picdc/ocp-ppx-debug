open Parsetree
open Asttypes
open Longident
open Ast_mapper
open Ast_helper

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

let add_debug_infos ?info exp enter leave =
  let loc = exp.pexp_loc in
  let enter = enter ?info exp in
  let leave = leave ?info exp in
  let exp' =
    Exp.let_ ~loc Nonrecursive
      [Vb.mk ~loc
          (Pat.var ~loc {loc; txt = "expr"})
          exp] @@
      Exp.sequence ~loc leave @@ Exp.ident {loc; txt = Lident "expr"}
  in
  Exp.sequence ~loc enter exp'

module type DebugIterator = sig
  val enter_fun : ?info:string -> expression -> expression
  val leave_fun : ?info:string -> expression -> expression

  val enter_match : ?info:string -> expression -> expression
  val leave_match : ?info:string -> expression -> expression

  val enter_let : ?info:string -> expression -> expression
  val leave_let : ?info:string -> expression -> expression

  val enter_for : ?info:string -> expression -> expression
  val leave_for : ?info:string -> expression -> expression

end

module DefaultIterator = struct
  let dummy exp =
    let loc = { txt = Lident "()"; loc = exp.pexp_loc} in
    { exp with pexp_desc = Pexp_construct (loc, None) }

  let enter_fun ?info exp = dummy exp
  let leave_fun ?info exp = dummy exp

  let enter_match ?info exp = dummy exp
  let leave_match ?info exp = dummy exp

  let enter_let ?info exp = dummy exp
  let leave_let ?info exp = dummy exp

  let enter_for ?info exp = dummy exp
  let leave_for ?info exp = dummy exp

end

module PrintIterator = struct

  let enter_fun ?info exp =
    print ?info exp "Entering fun"
  let leave_fun ?info exp =
    print ?info exp " Leaving fun"

  let enter_match ?info exp =
    print ?info exp "Entering match"
  let leave_match ?info exp =
    print ?info exp " Leaving match"

  let enter_let ?info exp =
    print ?info exp "Entering let"
  let leave_let ?info exp =
    print ?info exp " Leaving let"

  let enter_for ?info exp =
    print ?info exp "Entering for"
  let leave_for ?info exp =
    print ?info exp " Leaving for"

end

(* module DumpIterator = struct *)
(* TODO: write the output in a file *)
(* end  *)

module MakeIterator (Iter : DebugIterator) : sig

  val wrap_debug : Ast_mapper.mapper

end = struct
  open Ast_mapper
  open Iter

  let rec wrap_let_function ?fname m e =
    match e.pexp_desc with
      Pexp_function
        [{pc_rhs = { pexp_desc = Pexp_function _ | Pexp_fun _ } as e'} as c'] ->
          Exp.function_ [{c' with pc_rhs = wrap_let_function m e' ?fname}]
    | Pexp_fun (l, eopt, p, ({pexp_desc = Pexp_function _ | Pexp_fun _ } as e'))
      ->
      Exp.fun_ l eopt p (wrap_let_function ?fname m e')
    | Pexp_fun (l, eopt, p, e') ->
      let e'' = default_mapper.expr m e' in
      Exp.fun_ l eopt p
        (add_debug_infos ?info:fname e'' enter_fun leave_fun)
    | Pexp_function c ->
      let c' = List.map (default_mapper.case m) c in
      Exp.function_ @@ List.map (wrap_case ?fname m) c'
    | _ ->
      default_mapper.expr m e

  and wrap_case ?fname m c =
    let c' =
      { pc_lhs = default_mapper.pat m c.pc_lhs;
        pc_guard =
          (match c.pc_guard with
            Some g -> Some (default_mapper.expr m g)
          | None -> None);
        pc_rhs = default_mapper.expr m c.pc_rhs } in
    { c' with pc_rhs =
        add_debug_infos ?info:fname c'.pc_rhs enter_match leave_match}

  and wrap_debug =
    { default_mapper with
      case = wrap_case;
      value_binding =
        (fun m vb ->
          let vb' = default_mapper.value_binding m vb in
          let fname =
            match vb.pvb_pat.ppat_desc with
              Ppat_var { txt = l } -> Some l
            | _ -> None in
          { vb' with pvb_expr = wrap_let_function m vb'.pvb_expr ?fname });
    }

  let _ =
    register "debug" (fun _ -> wrap_debug)

end
