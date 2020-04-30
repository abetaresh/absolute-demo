
open Lang.Ast

let quantify_vars vars ty formula =
  let exists formula name =
    Exists (name, ty, formula) in
  List.fold_left exists formula (List.rev vars)

let dom_of_var ty l u v =
   And(
    Cmp (Var v, GEQ, Cst (l, ty)),
    Cmp (Var v, LEQ, Cst (u, ty)))

let dom_of_vars vars ty l u =
  List.map (dom_of_var ty l u) vars