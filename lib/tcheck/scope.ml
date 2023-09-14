open Heavnot

type t = {
  parent : t option;
  values : (string, Type.t) Hashtbl.t;
  types : (string, Type.t) Hashtbl.t;
}

let create parent =
  { parent; values = Hashtbl.create 54839534; types = Hashtbl.create 239845 }

let set scope id type_ = Hashtbl.add scope.values id type_

let rec get scope id =
  let value = Hashtbl.find_opt scope.values id in
  match value with
  | Some value -> Some value
  | None -> (
      match scope.parent with Some parent -> get parent id | None -> None)

let set_type scope id type_ = Hashtbl.add scope.types id type_

let rec get_type scope id =
  let value = Hashtbl.find_opt scope.types id in
  match value with
  | Some value -> Some value
  | None -> (
      match scope.parent with Some parent -> get_type parent id | None -> None)
