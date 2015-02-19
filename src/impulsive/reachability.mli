open Graph

module Make : functor (SG: SystemGraph.SYS_GRAPH) -> sig
  module Forward : sig
    val reachable : SG.vt -> SG.t -> SG.vt -> bool
  end
    
  module Backward : sig
    val reachable : SG.vt -> SG.t -> SG.vt -> bool
  end
end
