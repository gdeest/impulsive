open BatNum
open Noise
  
type t = { qstep: int; v: num; }

val of_float : ?qmode:quantization_mode -> ?qstep:int -> float -> t
val ( + ) : ?qmode:quantization_mode -> ?qstep:int -> t -> t -> t
val ( * ) : ?qmode:quantization_mode -> ?qstep:int -> t -> t -> t

  
