open SystemGraph
open Noise

module Make (SG : SYS_GRAPH) = struct
  module IR = ImpulseResponse.Make (SG)
  module R = Reachability.Make (SG)
  module HT = Hashtbl

  let propagate ~ndims ?(radius=50) ?(computeRanges=true) g =
    let nb_vertex = SG.nb_vertex g in

    let modelMap : (SG.vt, (SG.vt, (float list * float * float)) HT.t) HT.t
      = HT.create nb_vertex in
    let () = SG.iter_vertex (fun v ->
        HT.add modelMap v @@ HT.create nb_vertex) g in

    let compute_contribs v =
      let ir = IR.impulse_response g ~radius ~ndims v in
      let reachable = R.Backward.reachable v g in
      SG.iter_vertex (fun o ->
	  if reachable o then
	    let coeffs = ImpulseResponse.to_coeffs ndims ~radius:radius (ir o) in
	    let sum_k  = List.fold_left (+.) 0.0 coeffs and
	      sum_k2 = List.fold_left (fun sq k -> sq +. k ** 2.0) 0.0 coeffs in
            let model = HT.find modelMap o in
            print_endline "Blah";
	    HT.add model v ((if computeRanges then coeffs else []), sum_k, sum_k2)
	  else ()
        ) g in

    let () = SG.iter_vertex compute_contribs g in

    fun src_list v ->
      let model = HT.find modelMap v in
      let add_noises acc (src, noise_list) =
	try
	  let (coeffs, sum_k, sum_k2) = HT.find model src in
	  let add_noise acc noise =
	    let mean     = sum_k  *. noise.mean in
	    let variance = sum_k2 *. noise.variance in
	    let range =
	      if computeRanges then
		List.fold_left (fun (vlb, vub) k ->
		    let (a, b) = (k *. (fst noise.range), k *. (snd noise.range)) in
		    let (a, b) = if a > b then (b, a) else (a, b) in
		    (vlb +. a, vub +. b)
		  ) (0.0, 0.0) coeffs
	      else
		(0.0, 0.0)
	    in
	    noise_add acc {range=range; mean=mean; variance=variance} in
	  List.fold_left add_noise acc noise_list
	with
	  Not_found -> acc in
      List.fold_left add_noises nilNoise src_list
end
