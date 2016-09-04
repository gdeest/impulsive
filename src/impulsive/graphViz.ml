open SystemGraph

module type RENDER = sig
  module SG : SYS_GRAPH
  val string_of_var_id : SG.var_id -> string
  val string_of_operator_id : SG.operator_id -> string
end
  
module Make (M : RENDER) = struct
  include Graph.Graphviz.Dot
    (struct
      include M.SG
      let vertex_name v = match v with
	| Add s -> M.string_of_operator_id s
	| Sub s -> M.string_of_operator_id s
	| Mul (s, _) -> M.string_of_operator_id s
	| Var s -> M.string_of_var_id s

      let graph_attributes _ = []
      let default_vertex_attributes _ = []
      let vertex_attributes _ = []
      let default_edge_attributes _ = []
      let edge_attributes e =
	match e with
	| (_, (dep_vec, num_op, storage), _) ->
	  let rec join sep = function
	    | [] -> ""
	    | x::[] -> x
	    | x::xs -> x ^ sep ^ join sep xs in

	  let label =
            let dep_vec_str = match dep_vec with
              | NullVector -> "id"
              | Coords c -> "+(" ^ (join ", " (List.map string_of_int (Array.to_list c))) ^ "), " in
	    let num_op_str = string_of_int num_op in 
            let storage_str = match storage with
	      | hd::tl -> " | [" ^ (join ", " (List.map M.string_of_var_id storage)) ^"]"
	      | [] -> "" in
            dep_vec_str ^ num_op_str ^ storage_str
	  in
	  [`Label label]
      let get_subgraph _ = None
     end)

  let save_graph g f =
    let oc = open_out f in
    output_graph oc g    
end
