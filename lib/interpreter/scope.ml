
type t = {
  parent : t option;
  values : (string, Value.t) Hashtbl.t;
}

let create parent = {
  parent = parent;
  values = Hashtbl.create 543895;
};;

let set scope id value =
  Hashtbl.add scope.values id value
;;

let rec get scope id =
  let value = Hashtbl.find_opt scope.values id in
  match value with
  | Some value -> Some value
  | None ->
      match scope.parent with
      | Some parent -> get parent id
      | None -> None
;;

let print_values scope =
  Hashtbl.iter
    (fun k v -> print_endline (k ^ ": " ^ (Value.show v)))
    scope.values
;;
