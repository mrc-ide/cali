#' Calibrate
#'
#' @param parameters Other malariasimulation model parameters
#' @param target Values of target variable to calibrate to.
#' @param target_tt Timesteps of target.
#' @param summary_function A function that produces the a vector of the target variable.
#' @param tolerance The average tolerance, in target variable units, with which to match target.
#' @param interval The search interval.
#' @param ... Additional arguments to pass to the `uniroot()` function.
#'
#' @return Uniroot output
#' @export
calibrate <- function(parameters, target, target_tt, summary_function, tolerance, interval = c(0.01, 2000) / 365, ...){
  stats::uniroot(objective,
          parameters = parameters,
          target = target,
          target_tt = target_tt,
          summary_function = summary_function,
          tolerance = tolerance, 
          interval = interval, 
          ...)
}