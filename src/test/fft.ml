open Sfg


(* let c = Gc.get () *)
(* let _ = *)
(*   let open Gc in *)
(*   c.minor_heap_size <- (1024*1024 * 100); *)
(*   set c *)

  
module GV = GraphViz.Make(struct
  module SG = Sfg
  let sanitize str =
    let r = Str.regexp "\\[" in
    let str = Str.global_replace r "_" str in
    let r = Str.regexp "\\]" in
    Str.global_replace r "_" str
      
  let string_of_var_id (i,name) =
    (sanitize name) ^ "_" ^ (string_of_int i)
  let string_of_operator_id i = "op_" ^ (string_of_int i)
end)  
  
let g = read_idfix_sfg "fft_64.xml"

module IP = IntervalPropagation.Make (Sfg)
module NP = NoisePropagation.Make (Sfg)
  
(* let ranges = *)
(*   let count = ref 0 in  *)
(*   let ranges = Sfg.fold_vertex (fun v ranges -> *)
(*     match (Sfg.pred g v) with *)
(*     | [] -> count := !count + 1; (v, (-1.0,1.0))::ranges *)
(*     | _ -> ranges *)
(*   ) g [] in *)
(*   Printf.printf "Range count: %i\n" !count; *)
(*   ranges *)


  
(* let _ = *)
(*   print_endline "Saving graph..."; *)
(*   GV.save_graph g "fft_256.dot"   *)
    
(* let propag = *)
(*   print_endline "Preparing range propagation."; *)
(*   IP.propagate ~ndims:1 ~radius:1 g ranges *)


open Noise
let srcs = G.fold_vertex (fun v srcs ->
  (v, [compute_noise Infinite (Finite (-2))])::srcs
) g []

let _ = print_endline "Blah"

let propag = NP.propagate ~ndims:1 ~radius:10 ~computeRanges:false g srcs  



let print_noise noise =
  match noise.range with
  | (lb, ub) ->
    Printf.printf "Range: [%f; %f]; Mean: %f; Variance: %f\n" lb ub noise.mean noise.variance
    
let _ =
  Sfg.iter_vertex (fun v -> print_noise (propag v)) g

    


