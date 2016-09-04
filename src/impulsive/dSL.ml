open SystemGraph

module type GEN_IDS = sig
  include SYS_GRAPH

  val mk_var_id : unit -> var_id
  val mk_operator_id : unit -> operator_id 
end
  
module Make (M : GEN_IDS) = struct
  let g = ref M.empty
end
