remotes::install_github("mrc-ide/cali@joel")
library(cali)
library(malariasimulation)
library(ggplot2)

# Set up model run
parameters <- malariasimulation::get_parameters(list(human_population = 500))

# Define target
target <- c(0.3)
# Time point at which to match target
target_tt <- c(150)

# Other shared parameters
summary_function <- summary_pfpr_2_10

plot(summary_function(run_simulation(timesteps = 200, 
                                     parameters = parameters)),
     type = 'l', ylim = c(0, 1))

# Interval to look for EIR value in
interval <- c(1, 300)

set.seed(123)
out <- calibrate(parameters = parameters,
                 target = target,
                 target_tt = target_tt,
                 summary_function = summary_function,
                 tolerance = 0.02, 
                 interval = interval)

out$root

# Extra parameters
change_function <- eir_change
par_name <- "init_EIR"

# What is a change function?
default_change
eir_change

temp_par <- eir_change(x = 20, parameters = parameters, par_name = par_name)

temp_par$init_EIR

lines(summary_function(run_simulation(timesteps = 200, parameters = temp_par)), col = "red")

# Other new parameters
summary_name <- "pfpr_2_10"
# Transforms on the parameter to calibrate
trans.par = log
inv.trans.par = exp
# Transforms on the summary (the predictor in the gam model)
trans.summary = boot::logit
inv.trans.summary = boot::inv.logit

out_nl <- nlcali(parameters = parameters, 
                 target = target, 
                 target_tt = target_tt, 
                 summary_function = summary_pfpr_2_10, 
                 change_function = eir_change, 
                 par_name = par_name, 
                 summary_name = summary_name, 
                 interval = interval, 
                 ncores = 7, 
                 nsims = 100, 
                 trans.par = trans.par, 
                 inv.trans.par = inv.trans.par, 
                 trans.summary = trans.summary, 
                 inv.trans.summary = inv.trans.summary)

# Look at our answers!
out_nl$target_pred
out$root

# Simulation output
out_nl$sims

# Plot the simulations
ggplot(out_nl$sims) + 
  geom_point(aes(y = init_EIR, x = pfpr_2_10))

# EIR model fit
out_nl$fit

# Make a more detailed plot
p <- ggplot() + 
  geom_point(data = out_nl$sims, aes(y = init_EIR, x = pfpr_2_10)) +
  geom_ribbon(data = out_nl$fit,aes(x = pfpr_2_101, ymax = upr, ymin = lwr), 
              fill = "dodgerblue", alpha= 0.2) +
  geom_line(data = out_nl$fit, aes(x = pfpr_2_101, y = init_EIR), col = "dodgerblue")

print(p)

# Zoom in to where we care about and add in target + prediction
p + coord_cartesian(xlim = c(0, 0.5), ylim = c(0, 7.5)) +
  geom_vline(xintercept = target, col = "red") +
  geom_hline(yintercept = out_nl$target_pred$init_EIR, col = "red") + 
  geom_hline(yintercept = out_nl$target_pred$upr, col = "red", lty = 2)+ 
  geom_hline(yintercept = out_nl$target_pred$lwr, col = "red", lty = 2)


# Why transform?
ggplot() + 
  geom_point(data = out_nl$sims, aes(y = trans.par(init_EIR), x = trans.summary(pfpr_2_10))) +
  geom_ribbon(data = out_nl$fit,aes(x = trans.summary(pfpr_2_101), ymax = trans.par(upr), ymin = trans.par(lwr)), 
              fill = "dodgerblue", alpha= 0.2) +
  geom_line(data = out_nl$fit, aes(x = trans.summary(pfpr_2_101), y = trans.par(init_EIR)), col = "dodgerblue") + 
  geom_vline(xintercept = trans.summary(target), col = "red") +
  geom_hline(yintercept = trans.par(out_nl$target_pred$init_EIR), col = "red") + 
  geom_hline(yintercept = trans.par(out_nl$target_pred$upr), col = "red", lty = 2)+ 
  geom_hline(yintercept = trans.par(out_nl$target_pred$lwr), col = "red", lty = 2)

# Model object
out_nl$mod
summary(out_nl$mod)
coef(out_nl$mod)

# Do a run with our value
run1 <- summary_function(run_simulation(timesteps = 200, 
               parameters = change_function(parameters = parameters, 
                                            x = out_nl$target_pred$init_EIR, par_name = par_name)))

# Do a run with rootfinder value
run2 <- summary_function(run_simulation(timesteps = 200, 
                                        parameters = change_function(parameters = parameters, 
                                                                     x = out$root, par_name = par_name)))

plot(run1, type = "l", ylim = c(0, 0.4))
lines(run2, col = "red")
points(target_tt, target, pch = 16, col = "blue")
