#' Elimination check
#'
#' @param output Model run output
#' @param target Target
#'
#' @return Boolean indicator of unwanted elimination
check_elimination <- function(output, target){
  any(output == 0 & target != 0)
}

#' Interpolate EIR linearly
#'
#' @param eir Vector of EIRs
#' @param objective Vector of objectives
#'
#' @return Interpolated EIR
linear_interpolate <- function(eir, objective){
  eir <- sort(eir)
  objective <- sort(objective)
  b <- (eir[2] - eir[1]) / (objective[2] - objective[1])
  eir[1] - b * objective[1]
}

#' Check if the move has been successful
#'
#' @param objective Vector of objectives
#' @param direction Direction
#'
#' @return Boolean indicator of move success
good_move <- function(objective, direction){
  if(direction == "decrease"){
    good_move <- objective[2] <= 0
  } else {
    good_move <- objective[2] >= 0
  }
  return(good_move)
}

#' Check the fit with proposed EIR
#'
#' @param proposed_eir Proposed new EIR value
#' @inheritParams calibrate
#'
#' @return Boolean indicator of move success
check_fit <- function(proposed_eir, parameters, target, summary_function){
  parameters <- parameters |>
    malariasimulation::set_equilibrium(
      init_EIR = proposed_eir
    )
  simulation <- malariasimulation::run_simulation(
    timesteps = parameters$timesteps,
    parameters = parameters
  )
  output <- summary_function(simulation)
  
  output_print <- data.frame(
    Current = signif(output, 2),
    Target = signif(target, 2)
  )
  print(knitr::kable(output_print, format = "simple"))
  cat("\n")
  elimination <- check_elimination(output, target)
  objective <- ifelse(elimination, NA, sum(output - target))
  return(objective)
}

#' Update message
#'
#' @inheritParams linear_interpolate
print_update <- function(eir, objective){
  output_print <- data.frame(
    EIR = signif(eir, 2),
    Objective = signif(objective, 2)
  )
  print(knitr::kable(output_print, format = "simple"))
  cat("\n")
}