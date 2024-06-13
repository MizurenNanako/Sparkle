module CC2CL = struct
  open Concrete.CAST
  module S = Sexplib.Sexp

  let rec cl_of_expr (cc : cc_expr) =
    match cc.cc_expr_ty with
    | Mimport (name, _) -> S.Atom name
    | _ ->
      (match cc.cc_expr_desc with
       | CAF64 a -> S.Atom (string_of_float a)
       | CAI64 a -> S.Atom (Int64.to_string a)
       | CAStr a -> S.Atom (a |> String.escaped)
       | CAId a -> S.Atom a
       | CCall c ->
         S.List
           (cl_of_expr c.cc_call_callee
            :: List.map cl_of_expr c.cc_call_args)
       | CCond c ->
         let cond_br =
           c
           |> List.map
                (fun { cc_branch_pred = p; cc_branch_expr = e } ->
                   S.List [ cl_of_expr p; cl_of_expr e ])
         in
         let rec rewrite_last acc l =
           match l with
           | [] -> assert false
           | [ S.List [ _; e ] ] ->
             List.rev (S.List [ S.Atom "\'t"; e ] :: acc)
           | hd :: tl -> rewrite_last (hd :: acc) tl
         in
         S.List (S.Atom "cond" :: rewrite_last [] cond_br)
       | CLambda a ->
         let params =
           a.cc_func_param |> List.map (fun k -> S.Atom (fst k))
         in
         let body = cl_of_expr a.cc_func_body in
         S.List [ S.Atom "lambda"; S.List params; body ]
       | CTopBind b ->
         let answer =
           match b.cc_top_bind.cc_bound_value.cc_expr_desc with
           | CLambda lam ->
             let params =
               lam.cc_func_param |> List.map (fun k -> S.Atom (fst k))
             in
             let body = lam.cc_func_body |> cl_of_expr in
             S.List
               [ S.Atom "defun"
               ; S.Atom b.cc_top_bind.cc_bound_name
               ; S.List params
               ; body
               ]
           | _ ->
             S.List
               [ S.Atom "defconstant"
               ; S.Atom b.cc_top_bind.cc_bound_name
               ; cl_of_expr b.cc_top_bind.cc_bound_value
               ]
         in
         answer
       | CBind b ->
         let name = S.Atom b.cc_bind_bind.cc_bound_name in
         let value = b.cc_bind_bind.cc_bound_value |> cl_of_expr in
         let ctx = b.cc_bind_ctx |> cl_of_expr in
         S.List
           [ S.Atom "let"; S.List [ S.List [ name; value ] ]; ctx ]
       | CNop -> assert false)
  ;;

  let cl_of_module (mm : cc_module) =
    List.map cl_of_expr mm.cc_module_expr
  ;;
end
