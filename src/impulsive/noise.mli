type interval = float * float
  
type noise = { range : interval; mean : float; variance : float; }
  
val nilNoise : noise

val noise_add : noise -> noise -> noise
  
type precision = Infinite | Finite of int
    
type quantization_mode = Truncation | Rounding | ConvergentRounding
    
val truncation_noise : precision -> precision -> noise
  
val rounding_noise : ?convergent:bool -> precision -> precision -> noise
  
val compute_noise :
  ?mode:quantization_mode -> precision -> precision -> noise
