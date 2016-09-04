open Graph
open SystemGraph

module Make : functor (SG : SYS_GRAPH) -> sig
  val eval : SG.t -> ndims:int -> ?memoRadius:int -> (SG.vt -> int array -> float option) -> SG.vt -> int array -> float
end
