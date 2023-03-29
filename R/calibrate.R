#' Calibrate
#' 
#' General calibration function for malariasimulation. 
#'
#' @param parameters Other malariasimulation model parameters
#' @param target Values of target variable to calibrate to.
#' @param summary_function A function that take the raw model output as an argument and
#' produces a vector of the target variable.
#' @param tolerance The routine will complete when the average absolute weighted difference between the target variable 
#' and the target values falls below this value. Tolerance is specified in 
#' the units of the target variable (e.g. if my target variable is prevalence, and my tolerance is 0.05,
#' then the average absolute sum of the weighted difference between the target variable and the target values must be <5% for the routine to succeed).
#' @param weights A numerical vector of weights the same length as target giving the weights to use for elements of target.
#' @param elimination_check Check transmission is maintained for all target points with ongoing transmission before exiting early.
#' @param low Lower boundof EIRs
#' @param high Upper bound of EIRs
#' @param maxiter Maximum iterations. If stopping criteria have not been met by `maxiter`, the current best estimate will be returned
#'
#' @return Uniroot output
#' @export
calibrate <- function(target,
                      summary_function,
                      parameters,
                      tolerance,
                      weights = rep(1, length(target)),
                      elimination_check = TRUE,
                      maxiter = 20,
                      low = 0.001, high = 2000){

  x <- stats::uniroot(
    f = objective, 
    lower = low,
    upper = high,
    maxiter = maxiter,
    parameters = parameters,
    summary_function = summary_function,
    target = target,
    weights = weights,
    tolerance = tolerance,
    elimination_check = elimination_check
  )
  return(x$root)
}
