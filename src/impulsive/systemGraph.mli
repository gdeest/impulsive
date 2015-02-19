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

  include Sig.P with type V.t     = vt
		and  type V.label = vt
		and  type E.t     = vt * et * vt
		and  type E.label = et
end

module Make : functor (M : ID_TYPES) -> SYS_GRAPH
  with type var_id = M.var_id
  and  type operator_id = M.operator_id
