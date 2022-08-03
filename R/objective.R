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
  target_variable <- summary_function(raw_output)
  difference <- (target_variable - target)
  message("Current difference: ", paste(signif(difference, 3), collapse = " "))
  difference <- sum(difference * weights)
  message("Current sum of absolute weighted difference: ", paste(signif(abs(difference), 3), collapse = " "))
  # Adjust for specified tolerance
  difference[abs(difference) < tolerance] <- 0
  return(difference)
}