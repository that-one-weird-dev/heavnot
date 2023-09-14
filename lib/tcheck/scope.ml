open Heavnot

type t = {
  parent : t option;
  types : (string, Type.t) Hashtbl.t;
}

let create parent =
  { parent; types = (Hashtbl.create 54839534) }
;;

let set scope id type_ =
  Hashtbl.add scope.types id type_
;;

let rec get scope id =
  let value = Hashtbl.find_opt scope.types id in
  match value with
  | Some value -> Some value
  | None ->
      match scope.parent with
      | Some parent -> get parent id
      | None -> None
;;
