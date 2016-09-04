open Impulsive.SystemGraph
open Impulsive.Noise

module SG = struct
  include Impulsive.SystemGraph.Make (struct
    type var_id = string
    type operator_id = string
  end)

  let vertex_name v = match v with
    | Var name -> name
    | Mul (name, coeff) -> name
    | Add name -> name
end

module GV = Impulsive.GraphViz.Make (struct
  module SG = SG
  let string_of_var_id s = s
  let string_of_operator_id s = s
end)

module IR = Impulsive.ImpulseResponse.Make (SG)  

module IP = Impulsive.IntervalPropagation.Make (SG)

module NP = Impulsive.NoisePropagation.Make (SG)  
  
let make_filter a b =
  let open SG in
  let x = Var "x" in
  let y = Var "y" in
  let (_, g, last_add) = match b with
    | hd::tl ->
      let xb0 = Mul ("xb0", hd) in
      let g = add_edge empty xb0 x in
      List.fold_left (fun (i,g,prev) coeff ->
        let mul = Mul (("xb" ^ (string_of_int i)), coeff) in
        let add = Add ("add_xb_" ^ (string_of_int i)) in
        let g = add_edge_e g (mul, (Coords [|-i|], ["tmp_x"]), x) in
        let g = add_edge g add prev in
        let g = add_edge g add mul in
        (i+1, g, add)
      ) (1,g,xb0) tl
    | [] -> raise @@ Invalid_argument "Empty b list."
  in
  let (_, g, last_add) = List.fold_left (fun (i,g,prev) coeff ->
    let mul = Mul (("xa" ^ (string_of_int i)), coeff) in
    let add = Add ("add_xa_" ^ (string_of_int i)) in
    let g = add_edge_e g (mul, (Coords [|-i|], ["tmp_y"]), y) in
    let g = add_edge g add prev in
    let g = add_edge g add mul in
    (i+1, g, add)
  ) (1,g,last_add) a
  in
  let g = add_edge g y last_add in
  (g, x, y)


let (g,x,y) = make_filter [0.8] [1.0]

let () = GV.save_graph g "out.dot"

let () =
  let ir = IR.impulse_response g ~radius:10 x y in
  let coeffs = Impulsive.ImpulseResponse.to_coeffs 1 ~radius:10 ir in
  List.iter (fun f -> Printf.printf "%f\n" f) coeffs

let () =
  let propagate = IP.propagate ~ndims:1 ~radius:100 g [(x, (-1.0, 1.0))] in
  let print_range v =
    let (a,b) = propagate v in
    let range_str = Printf.sprintf "[%f, %f]" a b in
    print_endline  ("Range for "^ (SG.vertex_name v) ^ ": " ^ range_str)
  in
  SG.iter_vertex print_range g

let print_noise noise =
  match noise.range with
  | (lb, ub) ->
    Printf.printf "Range: [%f; %f]; Mean: %f; Variance: %f\n" lb ub noise.mean noise.variance
      
let propagate = NP.propagate ~ndims:1 ~radius:1000 ~computeRanges:true g
  
let show_with_prec p =
  let noise_list = SG.fold_vertex (fun v lst ->
    (v, [compute_noise Infinite (Finite p)])::lst
  ) g [] in
  let propagate_to = propagate noise_list in
  print_endline "------";
  SG.iter_vertex (fun v ->
    print_noise (propagate_to v)) g

let () =
  List.iter show_with_prec [-8; -7; -6; -5; -4; -3; -2; -1; 0; 1; 2]
