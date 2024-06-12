let rec tail l =
  match l with
  | [] -> raise @@ Failure "tail"
  | [ a ] -> a
  | _ :: tl -> tail tl
;;

let rec print_list dumper sp out l =
  match l with
  | [] -> ()
  | [ a ] -> Printf.fprintf out "%a" dumper a
  | a :: tl ->
    Printf.fprintf out "%a%s" dumper a sp;
    print_list dumper sp out tl
;;
