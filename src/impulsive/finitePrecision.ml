open Noise
  
type t = { qstep: int; v: BatNum.t; }

  
let quantize qmode v =
  let open BatNum in
  match qmode with
  | Truncation -> 			(* Truncate towards zero *)
    if (sign_num v < 0) then
      neg (floor (neg v))
    else
      floor v
  | Rounding ->
    round v
  | _ -> raise @@ Failure "Not implemented"
      
  
let of_float ?(qmode=Truncation) ?(qstep=(-8)) f =
  let open BatNum in
  let n = of_float f in
  let q = (of_int 2) ** (of_int qstep) in
  let v = quantize qmode (n / q) in
  { qstep; v }

let ( +)
    
    
