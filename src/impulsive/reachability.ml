open Graph

module ReachabilityCommon (M: sig val dir: Fixpoint.direction end) (SG: SystemGraph.SYS_GRAPH) = struct
  module R = Fixpoint.Make (SG)
    (struct
      type vertex = SG.vt  
      type edge = SG.E.t
      type g = SG.t
      type data = bool
      let direction = M.dir
      let equal = (=)
      let join = (||)
      let analyze _ x = x
     end)

  let reachable v = R.analyze ((=) v)
end
    
module Make (SG: SystemGraph.SYS_GRAPH) = struct
  module Forward =
    ReachabilityCommon (struct let dir = Fixpoint.Forward end) (SG)

  module Backward =
    ReachabilityCommon (struct let dir = Fixpoint.Backward end) (SG)
end

