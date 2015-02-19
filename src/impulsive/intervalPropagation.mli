open SystemGraph

type interval = float * float
  
module Make : functor (SG : SYS_GRAPH) -> sig
  val propagate : int -> (SG.vt * interval) list -> ?radius:int -> SG.t -> SG.vt -> interval
end
