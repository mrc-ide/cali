remotes::install_github("mrc-ide/cali@joel")
library(cali)
library(malariasimulation)
library(ggplot2)

# Big list of parameters
target <- c(0.3, 0.1, 0.2)
target_tt <- c(150, 250, 350)
summary_function <- summary_pfpr_2_10
interval <- c(1, 300)
change_function <- eir_change
par_name <- "init_EIR"
summary_name <- "pfpr_2_10"
trans.par = log
inv.trans.par = exp
trans.summary = boot::logit
inv.trans.summary = boot::inv.logit


parameters <- malariasimulation::get_parameters(list(human_population = 500))

parameters <- set_bednets(
  parameters = parameters,
  timesteps = 200,
  coverages = .75,
  retention = 5 * 365,
  dn0 = matrix(.533),
  rn = matrix(.56),
  rnm = matrix(.24),
  gamman = 2.64 * 365
)

plot(summary_function(run_simulation(timesteps = 400, parameters = parameters)), 
     type = 'l', ylim = c(0, 1))
points(target_tt, target, col = "red")
abline(v = 200, lty = 2)

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

# What is the outcome like?
out_nl$target_pred

lines(summary_function(run_simulation(timesteps = 400, 
                                     parameters = change_function(x = out_nl$target_pred$init_EIR, 
                                                                  parameters = parameters, 
                                                                  par_name = par_name))), 
     col = "blue")


# We can look at simulation output
ggplot(out_nl$sims) + 
  geom_point(aes(x = pfpr_2_10, y = init_EIR)) + 
  facet_wrap(.~timepoint)


# No fit for models over 2 dimensions!
out_nl$fit

# But you can use the model output and another package
library(mgcViz)
b <- getViz(out_nl$mod)
# Pick out first conditional smooth components
o <- plot( sm(b, 2) )
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()




