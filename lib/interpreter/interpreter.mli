module Value : sig
    type t

    val show : t -> string

    val unit : unit -> t
    val int : int -> t
    val float : float -> t
    val string : string -> t
    val external_function : (t list -> t) -> t
end

module Scope : sig
    type t

    val get : t -> string -> Value.t option
    val set : t -> string -> Value.t -> unit
end

val create_scope : unit -> Scope.t
val execute_root : Scope.t -> Heavnot.Ast.root -> unit
val execute_function : Scope.t -> string -> Value.t
