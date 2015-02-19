type interval = float * float
  
type noise = {
  range: interval;
  mean: float;
  variance: float;
}

let nilNoise = {
  range = (0.0, 0.0);
  mean = 0.0;
  variance = 0.0;
}
  
type precision =
| Infinite
| Finite of int
    
type quantization_mode =
| Truncation
| Rounding
| ConvergentRounding

(* Adding ***independent*** noises *)
let noise_add n1 n2 = {
  range = (fst n1.range +. fst n2.range, snd n1.range +. snd n2.range);
  mean = n1.mean +. n2.mean;
  variance = n1.variance +. n2.variance;
}
  
(* The formulas used below have been borrowed from Romuald Rocher's PhD thesis. *)
    
let truncation_noise p1 p2 = match (p1, p2) with
  | (_, Infinite) -> nilNoise
  | (Finite e1, Finite e2) ->
    if (e1 >= e2) then
      nilNoise
    else
      let q = 2.0 ** (float_of_int e2) in
      let k = float_of_int (e2 - e1) in {
	range = (-. q, 0.0);
	mean = q *. (1.0 -. 2.0 ** (-. k)) /. 2.0;
	variance = (q *. q) *. (1.0 -. 2.0 ** (-. 2.0 *. k)) /. 12.0;
      }
  | (Infinite, Finite e) ->
    let q = 2.0 ** (float_of_int e) in
    { range = (-. q, 0.0); mean =  -. (q /. 2.0); variance= (q ** 2.0) /. 12.0 }

let rounding_noise ?(convergent=false) p1 p2 = match (p1, p2) with
  | (_, Infinite) -> nilNoise
  | (Finite e1, Finite e2) ->
    if (e1 >= e2) then
      nilNoise
    else
      let q = 2.0 ** (float_of_int e2) in
      let k = float_of_int (e2 - e1) in
      if convergent then
	{
	  range = (-. q /. 2.0, q /. 2.0);
	  mean = 0.0;
	  variance = (q *. q) *. (1.0 -. 2.0 ** (-. 2.0 *. k +. 1.0)) /. 12.0;
	}
      else
	{
	  range = (-. q /. 2.0, q /. 2.0);
	  mean = q *. (2.0 ** (-. k)) /. 2.0;
	  variance = (q *. q) *. (1.0 -. 2.0 ** (-. 2.0 *. k)) /. 12.0;
	}
  | (_, Finite e) ->
    let q = 2.0 ** (float_of_int e) in
    { range = (-. q /. 2.0, q /. 2.0); mean = 0.0; variance= (q ** 2.0) /. 12.0 }
    
let compute_noise ?(mode=Truncation) p1 p2 =
  match mode with
  | Truncation -> truncation_noise p1 p2
  | Rounding -> rounding_noise ~convergent:false p1 p2
  | convergentRounding -> rounding_noise ~convergent:true p1 p2


