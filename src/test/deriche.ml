open Impulsive.SystemGraph
open Impulsive.Noise

module SG = struct
  include Impulsive.SystemGraph.Make (struct
    type var_id = string
    type operator_id = string
  end)

  let vertex_name v = match v with
    | Var name -> name
    | Sub name -> name
    | Mul (name, coeff) -> name
    | Add name -> name
end

module GV = Impulsive.GraphViz.Make (struct
  module SG = SG
  let string_of_var_id s = s
  let string_of_operator_id s = s
end)  

module IP = Impulsive.IntervalPropagation.Make (SG)
module NP = Impulsive.NoisePropagation.Make (SG)    
  
let deriche
    ~a1 ~a2 ~a3 ~a4 ~a5 ~a6 ~a7 ~a8
    ~b1 ~b2
    ~c1 ~c2
    =
  let g = SG.empty in
  
  (* Input *)
  let x = Var "x" in

  (* Horizontal pass - left to right *)
  let y11 = Var "y11" in
  
  let ma1 = Mul ("mul_a1", a1) and
      ma2 = Mul ("mul_a2", a2) and
      mb1 = Mul ("mul_b1_1", b1) and
      mb2 = Mul ("mul_b2_1", b2) in

  let g = SG.add_edge g ma1 x in
  let g = SG.add_edge_e g (ma2, (Coords [|0;-1|], 0, []), x) in
  let g = SG.add_edge_e g (mb1, (Coords [|0;-1|], 0, []), y11) in
  let g = SG.add_edge_e g (mb2, (Coords [|0;-2|], 0, []), y11) in    

  let pfir = Add "pfir_h1" and
      piir = Add "piir_h1" and
      pfir_iir = Add "pfir_iir_h1" in

  let g = SG.add_edge g pfir ma1 in
  let g = SG.add_edge g pfir ma2 in
  let g = SG.add_edge g piir mb1 in
  let g = SG.add_edge g piir mb2 in
  let g = SG.add_edge g pfir_iir pfir in
  let g = SG.add_edge g pfir_iir piir in

  let g = SG.add_edge g y11 pfir_iir in

  (* Horizontal pass - right to left *)
  let y12 = Var "y12" in  
  let ma3 = Mul ("mul_a3", a3) and
      ma4 = Mul ("mul_a4", a4) and
      mb1 = Mul ("mul_b2_1", b1) and
      mb2 = Mul ("mul_b2_2", b2) in

  let g = SG.add_edge_e g (ma3, (Coords [|0;1|], 0, []), x) in
  let g = SG.add_edge_e g (ma4, (Coords [|0;2|], 0, []), x) in
  let g = SG.add_edge_e g (mb1, (Coords [|0;1|], 0, []), y12) in
  let g = SG.add_edge_e g (mb2, (Coords [|0;2|], 0, []), y12) in    

  let pfir = Add "pfir_h2" and
      piir = Add "piir_h2" and
      pfir_iir = Add "pfir_iir_h2" in

  let g = SG.add_edge g pfir ma3 in
  let g = SG.add_edge g pfir ma4 in
  let g = SG.add_edge g piir mb1 in
  let g = SG.add_edge g piir mb2 in
  let g = SG.add_edge g pfir_iir pfir in
  let g = SG.add_edge g pfir_iir piir in

  let g = SG.add_edge g y12 pfir_iir in

  (* Merge horizontal passes *)
  let h = Var "h" in
  let add_h = Add "add_h" and
      mul_c1 = Mul ("mul_c1", c1) in
  let g = SG.add_edge g add_h y11 in
  let g = SG.add_edge g add_h y12 in
  let g = SG.add_edge g mul_c1 add_h in
  let g = SG.add_edge g h add_h in

  (* Vertical pass - top down *)
  let y21 = Var "y21" in
  
  let ma5 = Mul ("mul_a5", a5) and
      ma6 = Mul ("mul_a6", a6) and
      mb1 = Mul ("mul_b1_3", b1) and
      mb2 = Mul ("mul_b2_3", b2) in

  let g = SG.add_edge g ma5 h in
  let g = SG.add_edge_e g (ma6, (Coords [|-1;0|], 0, []), h) in
  let g = SG.add_edge_e g (mb1, (Coords [|-1;0|], 0, []), y21) in
  let g = SG.add_edge_e g (mb2, (Coords [|-2;0|], 0, []), y21) in    

  let pfir = Add "pfir_v1" and
      piir = Add "piir_v1" and
      pfir_iir = Add "pfir_iir_v1" in

  let g = SG.add_edge g pfir ma5 in
  let g = SG.add_edge g pfir ma6 in
  let g = SG.add_edge g piir mb1 in
  let g = SG.add_edge g piir mb2 in
  let g = SG.add_edge g pfir_iir pfir in
  let g = SG.add_edge g pfir_iir piir in

  let g = SG.add_edge g y21 pfir_iir in

  
  (* Vertical pass - bottom up *)
  let y22 = Var "y22" in
  let ma7 = Mul ("mul_a7", a7) and
      ma8 = Mul ("mul_a8", a8) and
      mb1 = Mul ("mul_b1_4", b1) and
      mb2 = Mul ("mul_b2_4", b2) in

  let g = SG.add_edge_e g (ma7, (Coords [|1;0|], 0, []), h) in
  let g = SG.add_edge_e g (ma8, (Coords [|2;0|], 0, []), h) in
  let g = SG.add_edge_e g (mb1, (Coords [|1;0|], 0, []), y22) in
  let g = SG.add_edge_e g (mb2, (Coords [|2;0|], 0, []), y22) in    

  let pfir = Add "pfir_v2" and
      piir = Add "piir_v2" and
      pfir_iir = Add "pfir_iir_v2" in

  let g = SG.add_edge g pfir ma7 in
  let g = SG.add_edge g pfir ma8 in
  let g = SG.add_edge g piir mb1 in
  let g = SG.add_edge g piir mb2 in
  let g = SG.add_edge g pfir_iir pfir in
  let g = SG.add_edge g pfir_iir piir in

  let g = SG.add_edge g y22 pfir_iir in
  

  (* Merge vertical passes *)
  let y = Var "y" in
  let add_y = Add "add_y" and
      mul_c2 = Mul ("mul_c2", c2) in
  let g = SG.add_edge g add_y y21 in
  let g = SG.add_edge g add_y y22 in
  let g = SG.add_edge g mul_c2 add_y in
  let g = SG.add_edge g y add_y in
  
  (g,x,y)


let deriche_smooth alpha = 
  let k = 
    ((1.0-.(exp(-.alpha)))**2.0)/.
      (1.0+.2.0*.alpha*.(exp(-.alpha))-.(exp(-.2.0*.alpha))) in
  deriche 
    ~a1:k
    ~a2:(k*.(exp(-.alpha))*.(alpha-.1.0))
    ~a3:(k*.(exp(-.alpha))*.(alpha+.1.0))
    ~a4:(-.k*.(exp(-2.0*.alpha)))
    ~a5:k
    ~a6:(k*.(exp(-.alpha))*.(alpha-.1.0))
    ~a7:(k*.(exp(-.alpha))*.(alpha+.1.0))
    ~a8:(-.k*.(exp(-.2.0*.alpha)))
    ~b1:(2.0*.(exp(-.alpha)))
    ~b2:(-.(exp(-.2.0*.alpha)))
    ~c1:1.0
    ~c2:1.0  

let deriche_xdiff alpha = 
  let k = 
    ((1.0-.(exp(-.alpha)))**2.0)/.
      (1.0+.2.0*.alpha*.(exp(-.alpha))-.(exp(-.2.0*.alpha))) in
  deriche 
    ~a1:0.0
    ~a2:1.0
    ~a3:(-1.0)
    ~a4:0.0
    ~a5:k
    ~a6:(k*.(exp(-.alpha))*.(alpha-.1.0))
    ~a7:(k*.(exp(-.alpha))*.(alpha+.1.0))
    ~a8:(-.k*.(exp(-.2.0*.alpha)))
    ~b1:(2.0*.(exp(-.alpha)))
    ~b2:(-.(exp(-.2.0*.alpha)))
    ~c1:(-.((1.0-.(exp(-.alpha)))**2.0))
    ~c2:1.0  

let deriche_ydiff alpha = 
  let k = 
    ((1.0-.(exp(-.alpha)))**2.0)/.
      (1.0+.2.0*.alpha*.(exp(-.alpha))-.(exp(-.2.0*.alpha))) in
  deriche 
    ~a1:k
    ~a2:(k*.(exp(-.alpha))*.(alpha-.1.0))
    ~a3:(k*.(exp(-.alpha))*.(alpha+.1.0))
    ~a4:(-.k*.(exp(-.2.0*.alpha)))
    ~a5:0.0
    ~a6:1.0
    ~a7:(-1.0)
    ~a8:0.0
    ~b1:(2.0*.(exp(-.alpha)))
    ~b2:(-.(exp(-.2.0*.alpha)))
    ~c1:1.0  
    ~c2:(-.((1.0-.(exp(-.alpha)))**2.0))

    
let (g,x,y) = deriche_smooth 1.0
(* let propagate_intervals = IP.propagate ~ndims:2 ~radius:5 g *)

(* let print_range prop v = *)
(*   let (a,b) = prop v in *)
(*   let range_str = Printf.sprintf "[%f, %f]" a b in *)
(*   print_endline  ("Range for "^ (SG.vertex_name v) ^ ": " ^ range_str) *)

(* let () = *)
(*   let intervals = [(x, (0.0, 255.0))] in *)
(*   SG.iter_vertex (print_range (propagate_intervals intervals)) g *)

(* let () = GV.save_graph g "deriche_smooth.dot" *)

let t1 = Sys.time ()
let propagate_noise = NP.propagate ~ndims:2 ~radius:100 ~computeRanges:false g
let t2 = Sys.time ()
let _ = Printf.printf "Exec time: %f\n" (t2 -. t1)

let print_noise noise =
  match noise.range with
  | (lb, ub) ->
    Printf.printf "Range: [%f; %f]; Mean: %f; Variance: %f\n" lb ub noise.mean noise.variance

let t1 = Sys.time ()

let show_with_prec p =
  let noise_list = SG.fold_vertex (fun v lst ->
    (v, [compute_noise Infinite (Finite p)])::lst
  ) g [] in
  let propagate_to = propagate_noise noise_list in
  print_endline "------";
  SG.iter_vertex (fun v ->
    print_noise (propagate_to v)) g

let () =  List.iter show_with_prec [-8]
   
let t2 = Sys.time ()
let _ = Printf.printf "Exec time: %f\n" (t2 -. t1)

(* let () = *)
(*   List.iter show_with_prec [-8; -7; -6; -5; -4; -3; -2] *)
