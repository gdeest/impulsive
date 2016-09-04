open SystemGraph

module Make : functor (SG : SYS_GRAPH) -> sig
  (* val impulse_response : SG.t -> ndims?radius:int -> SG.vt -> SG.vt -> *)
  (*   (int array -> float) *)
  val impulse_response: SG.t -> ?radius:int -> SG.vt -> ndims:int -> SG.vt -> int array -> float

end

val to_coeffs : int -> ?radius:int -> (int array -> float) -> float list
