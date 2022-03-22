#' Calibrate
#' 
#' General calibration function for malariasimulation. Please see the documentation for
#' \link[stats]{uniroot} to become familiar with possible additional arguments and convergence
#' errors.
#'
#' @param parameters Other malariasimulation model parameters
#' @param target Values of target variable to calibrate to.
#' @param target_tt Timesteps of target.
#' @param summary_function A function that take the raw model output as an argument and
#' produces a vector of the target variable.
#' @param tolerance The routine will complete when the difference between the target variable 
#' and each of the target values are all within this range. Tolerance is specified in 
#' the units of the target variable (e.g. if my target variable is prevalence, and my tolerance is 0.05,
#' then the difference between the target variable and each of the target values must be <5% for the routine to succeed).
#' @param interval The search interval of init EIRs.
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