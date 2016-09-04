open Graph
open SystemGraph
open Bigarray

let vec_add v1 v2 =
  let len = Array.length v1 in
  match v2 with
  | NullVector -> v1
  | Coords c -> 
    if len != Array.length c then
      raise @@ Invalid_argument "Incompatible vectors"
    else
      Array.init len (fun i -> v1.(i) + c.(i))

module Make (SG : SYS_GRAPH) = struct

  module HT = Hashtbl
    
  module S = Set.Make (struct
      type t = int array
      let compare = compare
    end)

  let any f arr = Array.fold_left (fun b i -> b || f i) false arr
      
  let eval g ~ndims ?(memoRadius=100) fixed_values =
    let memos = HT.create 100 in
    let vec_to_idx = Array.map (fun i -> i+memoRadius) in
    let vec_in_memo_window vec = not (any (fun i -> abs i > memoRadius) vec) in
    let rec eval node vec =
      match (fixed_values node vec) with
      | Some f -> f
      | None -> 
        let (memos, memoi) = try HT.find memos node with
            Not_found ->
            let nodeMemos = 
              let memos = Genarray.create Float32 C_layout (Array.make ndims (2*memoRadius+1)) in
              let memoi = Genarray.create Int C_layout (Array.make ndims (2*memoRadius+1)) in
              let _ = Genarray.fill memos 0.0; Genarray.fill memoi 0 in              
              (memos, memoi) in
            HT.add memos node nodeMemos ; nodeMemos in
        let idx = vec_to_idx vec and in_memo = vec_in_memo_window vec in
        if in_memo && (Genarray.get memoi idx = 1) then
          Genarray.get memos idx
        else
          let newResult =
            match node with
            | Add _ ->
              let succ_edges = SG.succ_e g node in
              let summands = List.map (fun (_,(dep_vec,_,_),s) ->
		  eval s (vec_add vec dep_vec)
                ) succ_edges in
              List.fold_left (+.) 0.0 summands
	    | Sub _ ->
	      let ((a,a_vec),(b,b_vec)) = match (SG.succ_e g node) with
		| [(_,(a_vec,0,_),a); (_,(b_vec,1,_),b)] -> ((a,a_vec), (b,b_vec))
		| [(_,(b_vec,1,_),b); (_,(a_vec,0,_),a)] -> ((a,a_vec), (b,b_vec))
		| _ -> raise @@ Invalid_argument "Malformed sub operands."
	      in
	      (eval a (vec_add vec a_vec)) -. (eval b (vec_add vec b_vec))
            | Mul (_,coeff) ->
              let multiplicand = match (SG.succ_e g node) with
                | [(_,(dep_vec,_,_), s)] -> eval s (vec_add vec dep_vec) 
                | _ -> raise @@ Invalid_argument "Wrong number of arguments"
              in
              coeff *. multiplicand

            | Var _ ->
              match (SG.succ_e g node) with
              | [(_,(dep_vec,_,_), s)] -> eval s (vec_add vec dep_vec) 
              (* A different input. *)
              | _ -> 0.0
          in
          if in_memo then
            Genarray.set memoi idx 1; Genarray.set memos idx newResult;
          newResult
    in
    eval
end
