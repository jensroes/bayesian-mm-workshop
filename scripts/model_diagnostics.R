# Load packages
library(tidyverse)
library(brms)

# Load posterior from earlier (should still be in your R's memory)
fit_brm <- readRDS(file = "stanout/brms_sim.rda")


# List of everything the model has estimates for (we only worry about the first two starting with "b_")
names(fit_brm$fit)


# Plot conditional means (marginal means)
conditional_effects(fit_brm)


# Overview of parameter estimates; this is useful for models with many slopes.
# Add the name of the model.
mcmc_plot(---)


# For example, here is how much faster or slower each participant was
mcmc_plot(---, pars = "^r_")


# Histogram of the posterior; type needs to be "hist" for histogram
mcmc_plot(---, pars = "b_conditionb", type = "---")


# Can you create a histogram for the intercept estimate?
# the intercept parameter is called "b_Intercept"
stanplot(---, pars = "---", type = "---")


# Did the model converge?
# ~~~~~~~~~~~~~~~~~~~~~~~

# R-hat should be smaller than 1.1
rhat(fit_brm) 

# Lets focus the parameters of interest called "b_Intercept" and "b_conditionb",
# which are the intercept and the slope respectively. Complete the R call:
rhat(fit_brm, pars = c("---", "---")) 


# Traceplot: we're looking for fat hairy cutterpillars, anything else is a problem.
# Create the plots for the intercept and slope (similar to the rhat call) and add "sigma"
# as well.
plot(fit_brm, pars = c("---", "---", "---"))


# Compare the posterior predicted data y_rep against the real data y
# In every iteration the model predicted data (given the estimated parameter values).
# For the data from 10 (randomly picked) iterations (i.e. samples).
pp_check(fit_brm, nsamples = 10)


# Use 100 samples.
pp_check(fit_brm, nsamples = ---)


# Use 500 samples.
pp_check(fit_brm, nsamples = ---)

