open SystemGraph
open Noise
  
module Make : functor (SG : SYS_GRAPH) -> sig
  val propagate :  int -> ?radius:int -> ?computeRange:bool -> SG.t -> (SG.vt * noise list) list -> SG.vt -> noise
end
