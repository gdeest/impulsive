module Noise : module type of Noise

module type ID_TYPES = SystemGraph.ID_TYPES
module type SYS_GRAPH = SystemGraph.SYS_GRAPH
module type RENDER = GraphViz.RENDER

type interval = float * float
  
module Make : functor (IdTypes : ID_TYPES) -> sig
  module SysGraph : SYS_GRAPH
    with type var_id = IdTypes.var_id
    and type operator_id = IdTypes.operator_id

  module Evaluation : sig
    val eval : SysGraph.t -> (SysGraph.vt -> int array -> float option) ->
      (SysGraph.vt -> int array -> float)      
  end

  module IntervalPropagation : sig
    val propagate : ndims:int -> ?radius:int -> SysGraph.t -> (SysGraph.vt * interval) list ->
      (SysGraph.vt -> interval)
  end

  module ImpulseResponse : sig
    val impulse_response : SysGraph.t -> ?radius:int -> SysGraph.vt -> SysGraph.vt ->
      (int array -> float)
	
    val to_coeffs : int -> ?radius:int -> (int array -> float) -> float list
  end

  module NoisePropagation : sig
    val propagate :  ndims:int -> ?radius:int -> ?computeRanges:bool -> SysGraph.t ->
      ((SysGraph.vt * Noise.noise list) list -> SysGraph.vt -> Noise.noise)
  end
end
