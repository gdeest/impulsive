open SystemGraph

type interval = float * float

module Make : functor (SG : SYS_GRAPH) -> sig
  val propagate : ndims:int -> ?radius:int -> SG.t -> (SG.vt * interval) list -> SG.vt -> interval
end
