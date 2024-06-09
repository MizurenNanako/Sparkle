let rec tail l =
  match l with
  | [] -> raise @@ Failure "tail"
  | [ a ] -> a
  | _ :: tl -> tail tl
