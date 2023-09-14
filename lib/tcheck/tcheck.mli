open Heavnot

module Scope : sig
    type t

    val create : t option -> t

    val get : t -> string -> Type.t option
    val set : t -> string -> Type.t -> unit
end

val check_root : Scope.t -> Heavnot.Ast.root -> unit
