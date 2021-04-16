# Load packages
library(tidyverse)
install.packages("bayestestR") # you probably need to install this
library(bayestestR)
library(brms)

# Load posterior from earlier (should still be in your R's memory)
fit_brm <- readRDS(file = "stanout/brms_sim.rda")

# List of everything the model has estimates for (we only worry about the first two starting with "b_")
names(fit_brm$fit)

# Extract slope parameter beta (difference between groups)
beta <- posterior_samples(fit_brm, pars = "b_conditionb") %>% pull()

# Calculate ROPE 
rope(beta) # default is a ROPE of -0.1 to 0.1

# Calculate ROPE for range -5 to 10
rope(beta, range = c(---,---))

# Calculate ROPE with no lower bound (a lower bound of -Inf) and an upper bound of 5
rope(beta, range = c(---,---))

# Calculate ROPE (with standardised effect size)
# The standardised effect is delta = beta / sigma
# Extract the variance sigma (replace ---)
sigma <- posterior_samples(fit_brm, pars = "sigma") %>% pull()

# Calculate the standardised effect of all posterior samples
delta <- beta / sigma

# Summarise the standardised effect size
posterior_summary(---)

# Calculate ROPE for negligible effects (i.e. inside of -0.1 and 0.1)
rope(---)

