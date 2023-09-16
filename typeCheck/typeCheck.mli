open Heavnot

module Scope : sig
    type t

    val get : t -> string -> Type.t option
    val set : t -> string -> Type.t -> unit
end

val create_scope : unit -> Scope.t
val check_root : Scope.t -> Heavnot.Ast.root -> unit
