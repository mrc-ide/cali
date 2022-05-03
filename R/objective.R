#' Objective function
#' 
#' Quantifies distance to target
#'
#' @param x Input parameter to be varied. For malariasimulation this is EIR.
#' @param parameters Other malariasimulation model parameters
#' @param target Values of target variable to calibrate to.
#' @param target_tt Timesteps of target.
#' @param summary_function A function that produces a vector of the target variable.
#' @param tolerance The average tolerance, in target variable units, with which to match target.
#'
#' @return Difference between output and target.
objective <- function(x, parameters, target, target_tt, summary_function, tolerance){
  message("Trying EIR: ", signif(x, 3))
  p <- malariasimulation::set_equilibrium(parameters, init_EIR = x)
  raw_output <- malariasimulation::run_simulation(timesteps = max(target_tt), parameters = p)
  target_variable <- summary_function(raw_output)
  difference <- (target_variable[target_tt] - target)
  message("Current difference: ", paste(signif(difference, 3), collapse = " "))
  # Adjust for specified tolerance
  difference[abs(difference) < tolerance] <- 0
  difference <- sum(difference)
  return(difference)
}