#' Objective function
#' 
#' Quantifies distance to target
#'
#' @param x Input parameter to be varied. For malariasimulation this is EIR.
#' @inheritParams calibrate
#'
#' @return Difference between output and target.
objective <- function(x, parameters, target, summary_function, tolerance, weights){
  message("Trying EIR: ", signif(x, 5))
  
  p <- malariasimulation::set_equilibrium(parameters, init_EIR = x)
  raw_output <- malariasimulation::run_simulation(timesteps = p$timesteps, parameters = p)
  model_output <- summary_function(raw_output)
  
  if(length(model_output) != length(target)){
    stop("summary function must produce a vector of the same length as target")
  }
  
  difference <- (model_output - target)
  weighted_difference <- difference * weights
  sum_weighted_difference <- sum(weighted_difference)
  
  print(signif(rbind(model_output, target, difference, weighted_difference)), 3)
  message("\nCurrent sum of weighted difference: ", signif(sum_weighted_difference, 3))
  
  # Adjust for specified tolerance
  sum_weighted_difference[abs(sum_weighted_difference) < tolerance] <- 0
  
  return(sum_weighted_difference)
}