open Graph
open SystemGraph

let vec_add v1 v2 =
  let len = Array.length v1 in
  if len != Array.length v2 then
    raise @@ Invalid_argument "Incompatible vectors"
  else
    Array.init len (fun i -> v1.(i) + v2.(i))

module Make (SG : SYS_GRAPH) = struct
  let eval g fixed_values =
    let memos = Hashtbl.create (SG.nb_vertex g) in
    let rec eval node vec =
      match (fixed_values node vec) with
      | Some f -> f
      | None -> 
        let nodeTable = try Hashtbl.find memos node with
            Not_found ->
              let nodeTable = Hashtbl.create 2500 in
              Hashtbl.add memos node nodeTable ; nodeTable in
        try Hashtbl.find nodeTable vec with
          Not_found ->
            let newResult =
              match node with
              | Add _ ->
                let succ_edges = SG.succ_e g node in
                let summands = List.map (fun (_,(dep_vec,_),s) ->
                  match dep_vec with
                  | NullVector -> eval s vec
                  | Coords c -> eval s (vec_add vec c)
                ) succ_edges in
                List.fold_left (+.) 0.0 summands
		  
              | Mul (_,coeff) ->
                let multiplicand = match (SG.succ_e g node) with
                  | [(_,(NullVector,_),s)] -> eval s vec
                  | [(_,(Coords c,_), s)] -> eval s (vec_add vec c) 
                  | _ -> raise @@ Invalid_argument "Wrong number of arguments"
                in
                coeff *. multiplicand
                  
              | Var _ ->
                match (SG.succ_e g node) with
                | [(_,(NullVector,_),s)] -> eval s vec
                | [(_,(Coords c,_), s)] -> eval s (vec_add vec c) 
                (* A different input. *)
                | _ -> 0.0
            in
            Hashtbl .add nodeTable vec newResult;
            newResult
    in
    eval
end
