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
                   p, e)
         in
         (* let rec rewrite_last acc l =
            match l with
            | [] -> assert false
            | [ S.List [ _; e ] ] ->
            List.rev (S.List [ S.Atom "\'t"; e ] :: acc)
            | hd :: tl -> rewrite_last (hd :: acc) tl
            in
            S.List (S.Atom "cond" :: rewrite_last [] cond_br) *)
         let rec reshape l =
           match l with
           | [] -> assert false
           | (p, e) :: [] ->
             S.List [ S.Atom "if"; cl_of_expr p; cl_of_expr e ]
           | (p, e) :: tl ->
             S.List
               [ S.Atom "if"; cl_of_expr p; cl_of_expr e; reshape tl ]
         in
         reshape cond_br
       | CLambda a ->
         let params =
           a.cc_func_param |> List.map (fun k -> S.Atom (fst k))
         in
         let body = cl_of_expr a.cc_func_body in
         S.List [ S.Atom "lambda"; S.List params; body ]
       | CBind b ->
         let name = S.Atom b.cc_bind_bind.cc_bound_name in
         let ctx = b.cc_bind_ctx |> cl_of_expr in
         (match b.cc_bind_bind.cc_bound_value with
          | { cc_expr_desc = CLambda { cc_func_param; cc_func_body }
            ; _
            } ->
            let defun =
              S.List
                [ S.Atom "defun"
                ; name
                ; S.List
                    (cc_func_param
                     |> List.map (fun k -> S.Atom (fst k)))
                ; cl_of_expr cc_func_body
                ]
            in
            S.List
              [ S.Atom "let"; S.List [ S.List [ name; defun ] ]; ctx ]
          | value ->
            S.List
              [ S.Atom "let"
              ; S.List [ S.List [ name; cl_of_expr value ] ]
              ; ctx
              ])
       | _ -> assert false)
  ;;

  let cl_of_module (mm : cc_module) = cl_of_expr mm.cc_module_expr
end
