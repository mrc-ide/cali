#' Transform bounded to unbounded
#'
#' @param x Value 
#' @param limits Vector of lower and upper limits
#'
#' @return Transformed value
to_real <- function(x, limits){
  log(x - limits[1]) - log(limits[2] - x)
}

#' Transform unbounded to bounded
#'
#' @param x Value 
#' @param limits Vector of lower and upper limits
#'
#' @return Transformed value
from_real <- function(x, limits){
  (limits[2] * exp(x) + limits[1]) / (1 + exp(x))
}

#' Propose a new value for EIR
#'
#' @param current_eir Current value
#' @param limits Inferred limits
#' @param direction "decrease" or "increase"
#' @param step The step size
#' @return New EIR value
proposal <- function(current_eir, limits, direction, step = 0.5) {
  if(current_eir < limits[1] | current_eir > limits[2]){
    stop("Current eir outside of limits")
  }
  r <- to_real(current_eir, limits)
  r_prop <- ifelse(direction ==  "decrease", r - step, r + step)
  proposed_eir <- from_real(r_prop, limits)
  return(proposed_eir)
}