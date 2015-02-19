open Graph
open SystemGraph

(* let vec_add v1 v2 = *)
(*   let len = Array.length v1 in *)
(*   if len != Array.length v2 then *)
(*     raise @@ Invalid_argument "Incompatible vectors" *)
(*   else *)
(*     Array.init len (fun i -> v1.(i) + v2.(i)) *)

let any f arr = Array.fold_left (fun b i -> b || f i) false arr
let all_zeros vec = not (any (fun i -> i != 0) vec)

module Make (SG : SYS_GRAPH) = struct
  module Eval = Evaluation.Make (SG)
  let impulse_response g ?(radius=50) i =
    let fixed_values v vec =
      if any ((fun i -> (abs i) > radius)) vec then
	Some 0.0
      else if v = i then
	Some (if all_zeros vec then 1.0 else 0.0)
      else
	None
    in
    Eval.eval g fixed_values
      
(* let memos = Hashtbl.create 100 in *)
(* let rec eval node vec = *)
(*   if any (fun i -> (abs i) > radius) vec then *)
(*     0.0 *)
(*   else *)
(*     let nodeTable = try Hashtbl.find memos node with *)
(*         Not_found -> *)
(*           let nodeTable = Hashtbl.create 2500 in *)
(*           Hashtbl.add memos node nodeTable ; nodeTable in *)
(*     try Hashtbl.find nodeTable vec with *)
(*       Not_found -> *)
(*         let newResult =  *)
(*           if node = i then *)
(*             if all_zeros vec then 1.0 *)
(*             else 0.0 *)
(*           else *)
(*             match node with *)
(*             | Add _ -> *)
(*               let succ_edges = SG.succ_e g node in *)
(*               let summands = List.map (fun (_,(dep_vec,_),s) -> *)
(*                 match dep_vec with *)
(*                 | NullVector -> eval s vec *)
(*                 | Coords c -> eval s (vec_add vec c) *)
(*               ) succ_edges in *)
(*               List.fold_left (+.) 0.0 summands *)
(*             | Mul (_,coeff) -> *)
(*               let multiplicand = match (SG.succ_e g node) with *)
(*                 | [(_,(NullVector,_),s)] -> eval s vec *)
(*                 | [(_,(Coords c,_), s)] -> eval s (vec_add vec c)  *)
(*                 | _ -> raise @@ Invalid_argument "Wrong number of arguments" *)
(*               in *)
(*               coeff *. multiplicand *)
          
(*             | Var _ -> *)
(*               match (SG.succ_e g node) with *)
(*               | [(_,(NullVector,_),s)] -> eval s vec *)
(*               | [(_,(Coords c,_), s)] -> eval s (vec_add vec c)  *)
(*                                                                     (\* A different input. *\) *)
(*               | _ -> 0.0 *)
(*         in *)
(*         Hashtbl .add nodeTable vec newResult; *)
(*         newResult  *)
(* in *)
(* eval o                 *)
		 end

(* Return (if any) the lexicographical successor of coords in set:
   [min,max] x  ... x [min,max] *)
  let increment coords min max =
    let rec increment inner coords =
      let len = Array.length coords in
      if len = 0 then None
      else begin
	let coord = coords.(0) and
            rest = Array.sub coords 1 (len-1) in
	if coord < max then
          let prefix = Array.init inner (fun _ -> min) in
          Some (Array.concat [prefix; [|coord+1|]; rest])
	else increment (inner+1) rest
      end
    in
    increment 0 coords

  let to_coeffs ndims ?radius:(r=50) ir =
    let rec to_coeffs coords lst = 
      match increment coords (-r) r with
      | None -> (ir coords)::lst
      | Some coords -> to_coeffs coords ((ir coords)::lst)
    in
    to_coeffs (Array.make ndims (-r)) []

