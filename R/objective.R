#' Objective function
#' 
#' Quantifies distance to target
#'
#' @param x Input parameter to be varied. For malariasimulation this is EIR.
#' @inheritParams calibrate
#'
#' @return Difference between output and target.
objective <- function(x, parameters, summary_function, target, weights, tolerance, elimination_check){
  message("\nTrying EIR: ", signif(x, 5))
  
  p <- malariasimulation::set_equilibrium(parameters, init_EIR = x)
  raw_output <- malariasimulation::run_simulation(timesteps = p$timesteps, parameters = p)
  model_output <- summary_function(raw_output)
  difference <- model_output - target
  weighted_difference <- difference * weights
  
  print(signif(rbind(model_output, target, difference, weighted_difference)), 3)
  
  if(elimination_check){
    bad_eliminated <- model_output == 0 & target != 0
    if(sum(bad_eliminated) > 0){
      message("Unwanted elimination")
      weighted_difference[bad_eliminated] <- -1e6
    }
  }
  
  mean_weighted_difference <- mean(weighted_difference)
  
  message("\nSum squared weighted difference: ", signif(mean_weighted_difference, 5))
  
  if(mean(abs(weighted_difference)) < tolerance){
    message("Mean absolute difference < tolerance")
    mean_weighted_difference <- 0
  }
  
  return(mean_weighted_difference)
}