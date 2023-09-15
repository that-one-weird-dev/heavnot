open Heavnot

let list_type () =
  Type.Object
    [
        "|List|", Type.Unit;
    ]

let list_new () = Type.Function { params = []; return = Type.Reference "List" }
let list_add () =
  Type.Function
    { params = [ Type.Reference "List"; Type.Unit ]; return = Type.Unit }

let register scope =
  Scope.set_type scope "List" (list_type ());
  Scope.set scope "list_new" (list_new ());
  Scope.set scope "list_add" (list_add ())
