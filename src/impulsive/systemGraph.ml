open Graph

type ('var_id, 'op_id) node =
| Add of 'op_id
| Mul of 'op_id * float
| Var of 'var_id


type dep_vector = 
| NullVector
| Coords of (int array)
    
type 'var_id edge_label = (dep_vector * 'var_id list)

module type ID_TYPES = sig
  type var_id
  type operator_id
end

module type SYS_GRAPH = sig
  type var_id
  type operator_id

  type vt = (var_id, operator_id) node
  type et = var_id edge_label

  include Sig.P with type V.t = vt
		and  type V.label = vt
		and  type E.t = vt * et * vt
		and  type E.label = et
end

module Make (M : ID_TYPES) = struct
  type var_id = M.var_id
  type operator_id = M.operator_id

  type vt = (M.var_id, M.operator_id) node
  type et = M.var_id edge_label

  module Vertex = struct
    type t      = vt
    let compare = Pervasives.compare
    let hash    = Hashtbl.hash
    let equal   = (=)
  end

  module Edge = struct
    type t      = et
    let compare = Pervasives.compare
    let default = (NullVector, [])
  end

  include Persistent.Digraph.ConcreteBidirectionalLabeled (Vertex) (Edge)
end

