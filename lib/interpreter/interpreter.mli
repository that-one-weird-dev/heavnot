module Value : sig
  type t = Value.t

  val show : t -> string
end

module Scope : sig
  type t

  val get : t -> string -> Value.t option
end

val execute : Heavnot.Ast.root -> Scope.t
val execute_function : Scope.t -> string -> Value.t
