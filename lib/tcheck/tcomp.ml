open Heavnot

let undefined_type id = raise (Failure ("Undefined type " ^ id))
let cant_compare_functions () = raise (Failure "Cant compare functions")

let get_type scope id =
  match Scope.get_type scope id with
  | Some type_ -> type_
  | None -> undefined_type id

[@@@warning "-27"]

let object_equal (a : Type.t Type.obj) (b : Type.t Type.obj) = false

let rec equal (scope : Scope.t) (a : Type.t) (b : Type.t) =
  match (a, b) with
  | Unit, Unit | Int, Int | Float, Float | String, String | Never, Never -> true
  | Object a, Object b -> object_equal a b
  | Reference a, Reference b ->
      equal scope (get_type scope a) (get_type scope b)
  | Reference a, b -> equal scope (get_type scope a) b
  | a, Reference b -> equal scope a (get_type scope b)
  | _ -> false

let not_equal scope a b = not (equal scope a b)

let list_diff_opt (scope : Scope.t) (a : Type.t list) (b : Type.t list) =
  List.find_opt (fun (a, b) -> not_equal scope a b) (List.combine a b)
