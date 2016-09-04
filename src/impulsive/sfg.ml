module G = SystemGraph.Make(struct
  type var_id = int * string
  type operator_id = int
end)

  
let str_pos pos =
  let line = Xml.line pos and
      (a,b) = Xml.range pos in
  Printf.sprintf "[line %d, chars: %d-%d]" line a b

type sfg_node =
| SysGraphNode of G.vt
| Const of float
| Eq
| Mul
| Sub
    
let find_attr t key =
  let open Xml in 
  let rec find = function
    | [] -> failwith @@ "Attr not found: " ^ key
    | hd::tl ->
      let k = attrib hd "key" in
      if k = key then
	attrib hd "value"
      else
	find tl
  in
  match t with
  | Element (_, _, cs) -> find cs
  | _ -> failwith "Tag should be an element."

open SystemGraph
    
let make_sfg tags =
  let open Xml in
  let idTbl = Hashtbl.create 1000 in
  let eqLHS = Hashtbl.create 1000 in
  let eqRHS = Hashtbl.create 1000 in
  let mulFactor = Hashtbl.create 1000 in
  let mulData = Hashtbl.create 1000 in
  let delayed = ref [] in
  let process_tag g tag =
    match tag with
    | Element ("node", _, _) ->
      let id = int_of_string @@ attrib tag "id" in
      let name = attrib tag "name" in
      let class_ = find_attr tag "CLASS" in begin
	match class_ with
	| "OP" ->
	  let nature = find_attr tag "NATURE" in
	  begin
	    match nature with
	    | "ADD" ->
	      let v = Add id in
	      Hashtbl.add idTbl id (SysGraphNode v);
	      G.add_vertex g v
	    | "SUB" ->
	      let v = Sub id in
	      Hashtbl.add idTbl id (SysGraphNode v);
	      G.add_vertex g v		
	    | "EQ" ->
	      Hashtbl.add idTbl id Eq; g
	    | "MUL" ->
	      Hashtbl.add idTbl id Mul; g
	    |  other -> failwith @@ "Unknown OP NATURE: " ^ other
	  end
	| "DATA" ->
	  let nature = find_attr tag "NATURE" in
	  if nature = "VARIABLE_VAL_UNKNOWN" then
	    (* Unknown values are bound to Var nodes. *)
	    let v = Var (id, name) in
	    Hashtbl.add idTbl id (SysGraphNode v);
	    G.add_vertex g v
	  else
	    let f = float_of_string @@ find_attr tag "VALUE" in
	    Hashtbl.add idTbl id (Const f);
	    g
	| s -> failwith @@ "Unknown class: " ^ s
      end
    | Element ("edge", _, _) ->
      let id_pred = int_of_string @@ attrib tag "id_pred" and
	  id_succ = int_of_string @@ attrib tag "id_succ" in
      let src = Hashtbl.find idTbl id_pred and
	  dst = Hashtbl.find idTbl id_succ in begin
	    match (src, dst) with
	    | (SysGraphNode v1, SysGraphNode v2) ->
	      begin
		match v2 with
		| Sub _ ->
		  let num_op = int_of_string @@ find_attr tag "NUM_INPUT" in
		  G.add_edge_e g (v2,(NullVector, num_op, []),v1)
		| _ -> G.add_edge g v2 v1
	      end
	    | (Eq, _) -> Hashtbl.add eqLHS id_pred id_succ; g
	    | (_, Eq) -> Hashtbl.add eqRHS id_succ id_pred; g
	    | (Const f, Mul) -> Hashtbl.add mulFactor id_succ f; g
	    | (SysGraphNode v, Mul) -> Hashtbl.add mulData id_succ v; g
	    | (o1, o2) ->
	      let num_op = begin
		match o2 with
		| SysGraphNode (Sub _) ->
		  Some (int_of_string @@ find_attr tag "NUM_INPUT")
		| _ -> None
	      end in
	      delayed := (id_succ,id_pred,num_op)::(!delayed); g
	  end
    | _ -> failwith "Encountered PCData"
  in 
  let g = List.fold_left process_tag G.empty tags in
  let g =
    Hashtbl.fold (fun id factor g ->
      let v = Mul (id, factor) in
      Hashtbl.add idTbl id (SysGraphNode v);
      G.add_vertex g v
    ) mulFactor g in
  let g =
    Hashtbl.fold (fun id mulData g ->
      let (SysGraphNode v1) = Hashtbl.find idTbl id in
      G.add_edge g v1 mulData
    ) mulData g in
  let g =
    Hashtbl.fold (fun id lhsId g ->
      let (SysGraphNode v1) = Hashtbl.find idTbl lhsId in
      let (SysGraphNode v2) = Hashtbl.find idTbl (Hashtbl.find eqRHS id) in
      G.add_edge g v1 v2
    ) eqLHS g in
  let g =
    List.fold_left (fun g (id1,id2,num_op) ->
      let (SysGraphNode v1) = Hashtbl.find idTbl id1 in
      let (SysGraphNode v2) = Hashtbl.find idTbl id2 in
      match num_op with
      | Some i -> G.add_edge_e g (v1,(NullVector,i,[]),v2)
      | _ -> G.add_edge g v1 v2
    ) g !delayed in 
  g

let read_idfix_sfg f =
  let open Xml in
  let xml =
    try parse_file f
    with
    | Error (msg, pos) -> failwith @@ (error_msg msg) ^ " " ^ (str_pos pos)
    | File_not_found str -> failwith ("File not found: " ^ str)
  in
  match xml with
  | Element ("graph", _, children) ->
    make_sfg children
  | _ -> failwith "Should not have seen that."

include G


