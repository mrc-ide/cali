#' Equilibrium PfPr
#' 
#' Estimate equilibrium pfpr prevalence for a given eir and treatment coverage
#'
#' @param eir EIR for adults, in units of infectious bites per person per year  
#' @param ft The proportion of clinical cases effectively treated
#'
#' @return Plamodium falciparum prevalence in 2-10 year olds
pfpr_2_10 <- function(eir, ft = 0.1) {
  eq <- malariaEquilibrium::human_equilibrium(
    eir,
    ft = ft,
    p = malariaEquilibrium::load_parameter_set("Jamie_parameters.rds"),
    age = 0:100
  )
  sum(eq$states[3:11, 'pos_M']) / sum(eq$states[3:11, 'prop'])
}

#' Equilibrium Objective function
#' 
#' Used to estimate the starting EIR for a given PfPr
#'
#' @inheritParams pfpr_2_10 
#' @param target_pfpr Target PfPr in 2-10 year olds
#'
#' @return The difference between current and target prevalence
eq_objective <- function(eir, target_pfpr, ft){
  pfpr_2_10(eir, ft) - target_pfpr
}

#' Estimate the EIR for a target PfPr
#' 
#' Estimate the equilibrium EIR that results in a specified Plasmodium falciparum
#' prevalence in 2-10 year olds for a given treatment coverage
#'
#' @inheritParams eq_objective
#'
#' @return EIR
#' @export
#'
#' @examples
#' get_eq_eir(target_pfpr = 0.2, ft = 0.4)
get_eq_eir <- function(target_pfpr, ft = 0.1){
  if(target_pfpr <= 0 | target_pfpr > 0.9){
    stop("eq_pfpr_2_10 must be between 0 and 0.9")
  }
  if(ft < 0 | ft >= 1){
    stop("ft must be between 0 and 1")
  }

  opt <- stats::uniroot(
    f = eq_objective,
    lower = .Machine$double.eps,
    upper = 200,
    target_pfpr = target_pfpr,
    ft = ft,
    extendInt = "upX"
  )
  opt$root
}