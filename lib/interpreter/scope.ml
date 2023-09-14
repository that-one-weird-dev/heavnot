open Heavnot

type t = {
  parent : t option;
  values : (string, Value.t) Hashtbl.t;
  types : (string, Type.t) Hashtbl.t;
}

let create parent =
  { parent; values = Hashtbl.create 543895; types = Hashtbl.create 83124 }

let set scope id value = Hashtbl.add scope.values id value

let rec get scope id =
  let value = Hashtbl.find_opt scope.values id in
  match value with
  | Some value -> Some value
  | None -> (
      match scope.parent with Some parent -> get parent id | None -> None)

let print_values scope =
  Hashtbl.iter (fun k v -> print_endline (k ^ ": " ^ Value.show v)) scope.values

let set_type scope id type_ = Hashtbl.add scope.types id type_

let rec get_type scope id =
  let value = Hashtbl.find_opt scope.types id in
  match value with
  | Some value -> Some value
  | None -> (
      match scope.parent with Some parent -> get_type parent id | None -> None)
