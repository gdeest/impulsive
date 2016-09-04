module type ID_TYPES = SystemGraph.ID_TYPES
module type SYS_GRAPH = SystemGraph.SYS_GRAPH
module type RENDER = GraphViz.RENDER

module Noise = Noise
  
module Make (IDs : ID_TYPES) = struct
  module SysGraph = SystemGraph.Make (IDs)
  module Evaluation = Evaluation.Make (SysGraph)
  module IntervalPropagation = IntervalPropagation.Make (SysGraph)
  module ImpulseResponse = struct
    include ImpulseResponse.Make (SysGraph)      
    let to_coeffs = ImpulseResponse.to_coeffs
  end
  module NoisePropagation = NoisePropagation.Make (SysGraph)
end

module Simple = struct
  module M = Make (struct
    type var_id = string
    type operator_id = string
  end)

  module GraphViz = GraphViz.Make (struct
    module SG = M.SysGraph
    let string_of_var_id s = s
    let string_of_operator_id s = s
  end)

  include M
end
