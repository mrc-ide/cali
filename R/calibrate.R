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
#' @param elimination_check Check transmission is maintained for all target points with ongoing transmission before exiting early.
#' @param low Lower boundof EIRs
#' @param high Upper bound of EIRs
#' @param maxiter Maximum iterations
#'
#' @return Uniroot output
#' @export
calibrate <- function(target,
                      objective_function,
                      summary_function,
                      parameters,
                      tolerance,
                      weights = rep(1, length(target)),
                      elimination_check = TRUE,
                      maxiter = 20,
                      low = 0.001, high = 2000){
  
  x <- proposal(low, high)
  
  for(i in 1:maxiter){
    if(low == high) break
    
    y <- objective(x = x,
                   parameters = parameters,
                   summary_function = summary_function)
    difference <- y - target
    weighted_difference <- difference * weights
    
    print(signif(rbind(y, target, difference, weighted_difference)), 3)
    diff <- sum(weighted_difference)
    
    # Can stop early if close enough and transmission maintained
    within_tol <- abs(diff) < tolerance
    transmisison <- TRUE
    if(elimination_check){
      transmission <- all(y[target > 0] > 0)
    }
    if(within_tol & transmission) break
    
    if(diff < 0){
      low <- x
      x <- proposal(x, high)
    }
    if(diff > 0){
      high <- x
      x <- proposal(low, x)
    }
  }
  return(x)
}

#' Propose new EIR, moving on log scale
#'
#' @param a lower EIR
#' @param b upper EIR
#'
#' @return EIR
proposal <- function(a, b){
  exp(log(a) + (log(b) - log(a)) / 2)
}