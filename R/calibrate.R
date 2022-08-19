#' Calibrate
#' 
#' General calibration function for malariasimulation. Please see the documentation for
#' \link[stats]{uniroot} to become familiar with possible additional arguments and convergence
#' errors.
#'
#' @param parameters Other malariasimulation model parameters
#' @param target Values of target variable to calibrate to.
#' @param summary_function A function that take the raw model output as an argument and
#' produces a vector of the target variable.
#' @param tolerance The routine will complete when the sum of the absolute weighted difference between the target variable 
#' and the target values falls below this value. Tolerance is specified in 
#' the units of the target variable (e.g. if my target variable is prevalence, and my tolerance is 0.05,
#' then the sum of the absolute weighted difference between the target variable and the target values must be <5% for the routine to succeed).
#' @param weights A numerical vector of weights the same length as target giving the weights to use for elements of target.
#' @param interval The search interval of init EIRs.
#' @param elimination_penalty If set to a value then any evaluations of the objective function where the model output is
#' 0 (i.e. elimination) and target >0 will be given this value. Used to force a solution to have ongoing transmission.
#' @param maxiter Max iterations
#' @param ... Additional arguments to pass to the `f` argument` of the `uniroot()` function.
#'
#' @return Uniroot output
#' @export
calibrate <- function(parameters, target, summary_function, tolerance,
                      weights = rep(1, length(target)),
                      interval = c(0.01, 2000) / 365, elimination_penalty = NULL,
                      maxiter = 20,
                      ...){
  stats::uniroot(objective,
          parameters = parameters,
          target = target,
          summary_function = summary_function,
          weights = weights,
          tolerance = tolerance, 
          interval = interval,
          elimination_penalty = elimination_penalty,
          maxiter = maxiter,
          ...)
}