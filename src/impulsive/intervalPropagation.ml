open SystemGraph

type interval = float * float  

module Make (SG : SYS_GRAPH) = struct
  module IR = ImpulseResponse.Make (SG)
  module R = Reachability.Make (SG)

  module HT = Hashtbl

  let propagate ~ndims ?(radius=50) g ranges =
    (* irMap contains impulse propagation function for each node for which a range is specifed. *)
    let irMap = HT.create 100 in
    let () = List.iter (fun (v, _) ->
	let ir = IR.impulse_response g ~radius ~ndims v in
	HT.add irMap v ir) ranges in

    (* Computes the propagation of interval (lb,ub) from node from to v *)
    let propag v (from, (lb, ub)) =
      let ir = HT.find irMap from in
      let coeffs = ImpulseResponse.to_coeffs ndims ~radius:radius (ir v) in
      List.fold_left (fun (vlb, vub) k ->
	  let (a, b) = (k *. lb, k *. ub) in
	  let (a, b) = if a > b then (b, a) else (a, b) in
	  (vlb +. a, vub +. b)
	) (0.0, 0.0) coeffs in

    (* Given node v, compute the sum of propagated ranges. *)
    fun v ->
      List.fold_left (fun (vlb, vub) range ->
	  let (lb, ub) = propag v range in
	  (vlb +. lb, vub +. ub)) (0.0, 0.0) ranges
end
