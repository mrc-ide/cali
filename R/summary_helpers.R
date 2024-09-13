#' Prevalence 2-10 year olds
#'
#' @param x malariasimualtion model output
#'
#' @return A vector of PfPR 2-10.
#' @export
summary_mean_pfpr_2_10 <- function(x){
  prev_2_10 <- mean(x$n_detect_lm_730_3650 / x$n_age_730_3650)
  return(prev_2_10)
}