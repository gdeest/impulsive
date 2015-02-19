open SystemGraph

module type RENDER = sig
  module SG : SYS_GRAPH
  val string_of_var_id : SG.var_id -> string
  val string_of_operator_id : SG.operator_id -> string
end

module Make : functor (M : RENDER) -> sig
  val save_graph : M.SG.t -> string -> unit
end
