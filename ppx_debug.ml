open Parsetree
open Asttypes
open Longident
open Ast_mapper
open Ast_helper


let print loc ?info at_begin s =
  let s = match info with None -> s | Some f -> s ^ " " ^ f in
  let print =
    Exp.apply ~loc
      (Exp.ident ~loc { loc; txt = Ldot (Lident "Format", "eprintf"); })
      ["", Exp.constant ~loc
         (if at_begin then
            (Const_string (Printf.sprintf "%s %%s\n%!" s, None))
         else (Const_string (Printf.sprintf "%s %%s\n%!" s, None)));
       "", Exp.ident ~loc ({ loc; txt = Ldot (Lident "Pervasives", "__LOC__"); })
      ] in
  Exp.ifthenelse ~loc
    (Exp.apply ~loc
       (Exp.ident ~loc { loc; txt = Ldot (Lident "Pervasives",  "<>") })
       [("", Exp.constant (Const_string ("", None)));
        ("", Exp.apply ~loc
           (Exp.ident ~loc { loc; txt = Ldot (Lident "Sys", "getenv") })
           [("", Exp.constant (Const_string ("PPX_DEBUG", None)))])])
    print
    None

let add_debug_informations ?info expr =
  let loc = expr.pexp_loc in
  let before = print loc ?info true "before" in
  let after = print loc ?info false "after" in
  let expr' =
    Exp.let_ ~loc Nonrecursive
      [Vb.mk ~loc
         (Pat.var ~loc {loc; txt = "expr"})
         expr] @@
    Exp.sequence ~loc after @@ Exp.ident {loc; txt = Lident "expr"}
  in
  Exp.sequence ~loc before expr'

let rec add_let_function ?fname m e =
  match e.pexp_desc with
    Pexp_function
      [{pc_rhs = { pexp_desc = Pexp_function _ | Pexp_fun _ } as e'} as c'] ->
    Exp.function_ [{c' with pc_rhs = add_let_function m e' ?fname}]
  | Pexp_fun (l, eopt, p, ({pexp_desc = Pexp_function _ | Pexp_fun _ } as e')) ->
    Exp.fun_ l eopt p (add_let_function ?fname m e')
  | Pexp_fun (l, eopt, p, e') ->
    let e'' = default_mapper.expr m e' in
    Exp.fun_ l eopt p (add_debug_informations ?info:fname e'')
  | Pexp_function c ->
    let c' = List.map (default_mapper.case m) c in
    Exp.function_ @@ List.map (add_case ?fname m) c' 
  | _ ->
    default_mapper.expr m e
      
and add_case ?fname m c =
  let c' =
    { pc_lhs = default_mapper.pat m c.pc_lhs;
      pc_guard =
        (match c.pc_guard with
           Some g -> Some (default_mapper.expr m g)
         | None -> None);
      pc_rhs = default_mapper.expr m c.pc_rhs } in
  { c' with pc_rhs = add_debug_informations ?info:fname c'.pc_rhs } 
  
                                 
and mapper =
  { default_mapper with
    expr =
      (fun m e -> e);
        (* match e.pexp_desc with *)
        (*     Pexp_let (rf, vbs, e) -> *)
        (*     let vbs' = List.map (default_mapper.value_binding m) vbs in *)
        (*     let e' = default_mapper.expr m e in *)
        (*     Exp.let_ ~loc:e.pexp_loc rf vbs' e' *)
        (*   | _ -> default_mapper.expr m e); *)
    case = add_case;
    value_binding =
      (fun m vb ->
         let vb' = default_mapper.value_binding m vb in
         let fname =
           match vb.pvb_pat.ppat_desc with
             Ppat_var { txt = l } -> Some l
           | _ -> None in
         { vb' with pvb_expr = add_let_function m vb'.pvb_expr ?fname });
  }

         
let _ =
  register "debug" (fun _ -> mapper)
