remotes::install_github("mrc-ide/cali@joel")
library(cali)
library(malariasimulation)
library(ggplot2)
summary_function <- summary_pfpr_2_10

parameters <- malariasimulation::get_parameters(list(human_population = 2000))

parameters <- set_bednets(
  parameters = parameters,
  timesteps = 100,
  coverages = .95,
  retention = 1 * 365,
  dn0 = matrix(.533),
  rn = matrix(.56),
  rnm = matrix(.24),
  gamman = 365
)

parameters <- malariasimulation::set_equilibrium(parameters, init_EIR = 10)

set.seed(1234)
run <- summary_function(run_simulation(timesteps = 1500, parameters = parameters))
plot(run,
     type = "l", xlab = "Time")
abline(v = 100, lty = 2)

# BASELINE
points(x = 90, y =run[90], col = "red", pch = 16)
# SURVEY
points(x = 550, y  = 0.25, col = "blue", pch = 16)

# Big list of parameters
target <- c(0.25)
target_tt <- c(550)
summary_name <- "pfpr_2_10"

# Custom change function example due to matrix
parameters$bednet_gamman

example_change_function <- function(x, parameters, par_name){
  parameters[[par_name]] <- matrix(x, 1, 1)
  return(parameters)
}

example_change_function(0.1, parameters, "bednet_dn0")$bednet_dn0

# We can use default because bednet_gamman is not a matrix etc
change_function <- default_change
par_name <- "bednet_gamman"
interval <- c(365, 365*3)


res <- run_simulations(parameters = parameters, 
                       target_tt = target_tt, 
                       interval = interval, 
                       nsims = 100, 
                       par_name = par_name, 
                       summary_name = summary_name, 
                       ncores = 7, 
                       summary_function = summary_function, 
                       change_function = change_function)


ggplot(res) + 
  geom_point(aes(x = pfpr_2_10, y = bednet_gamman))

# No need to bother with a transformation by the look of it!

out <- nlcali(parameters = parameters, 
              target = target, 
              target_tt = target_tt, 
              summary_function = summary_function, 
              change_function = change_function, 
              par_name = par_name, 
              summary_name = summary_name, 
              interval = interval, 
              ncores = 7, 
              nsims = 100)

out$target_pred

plot(run,
     type = "l", xlab = "Time")
abline(v = 100, lty = 2)
# BASELINE
points(x = 90, y =run[90], col = "red", pch = 16)
# SURVEY
points(x = 550, y  = 0.25, col = "blue", pch = 16)
lines(summary_function(run_simulation(timesteps = 1500, 
                                      parameters = change_function(x = out$target_pred$bednet_gamman, 
                                                                   parameters = parameters, 
                                                                   par_name = par_name))), 
      col = "green")

ggplot() + 
  geom_ribbon(data = out$fit, aes(x = pfpr_2_101, ymin = lwr, ymax = upr), fill = "blue", alpha = 0.2) +
  geom_point(data = out$sims, aes(x = pfpr_2_10, y = bednet_gamman)) +
  geom_line(data = out$fit, aes(x = pfpr_2_101, y = bednet_gamman), col = "blue")

# Evaluating calibration and accounting for randomness

# Old gamman value
old <- score_calibration(parameters = parameters, 
                  target = target, 
                  target_tt = target_tt, 
                  nsims = 100, 
                  summary_function = summary_function)

# New gamman value
new <- score_calibration(parameters = change_function(x = out$target_pred$bednet_gamman,
                                               parameters = parameters, 
                                               par_name = par_name), 
                  target = target, 
                  target_tt = target_tt, 
                  nsims = 100, 
                  summary_function = summary_function)

old$score
new$score

ggplot(rbind(old$sims[, lab := "old"], new$sims[, lab := "new"])) + 
  geom_histogram(aes(x = sf, fill = lab)) +
  geom_vline(xintercept = target, lty = 2) + 
  labs(x = "Prevalence at observation time") + 
  theme_classic() +
  scale_fill_discrete(name = "Parameter")
