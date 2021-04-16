# Load packages
library(tidyverse)
library(brms)

# Load model
fit_brm <- readRDS(file = "stanout/brms_sim_with_prior.rda")

# Check the priors that were used.
# Task: apply the function below to the model object, you loaded above.
prior_summary(---)

# Find the name of the slope parameter (our effect of interest)
names(fit_brm$fit)

# but ignore the prefix b_ for the below.
# Calculate BF for the null hypothesis being true
hypothesis(fit_brm, "conditionb = 0")

# Evid.Ratio and Post.Prob are NA.
# Reason is, we didn't save the prior when fitting the model (panic now!).
# So we have to get our own hands dirty.

# Extract posterior for beta
# Task: complete the name of the slope coefficient
beta <- posterior_samples(fit_brm, pars = "b_---") %>% pull()

# This is described in Nicenboim and Vasishth (2016): to determine the height
# of the posterior at 0 we use logspline:
library(polspline)
fit_posterior <- logspline(beta) # determine log-density
posterior <- dlogspline(0, fit_posterior) # determine the posterior density at 0
prior <- dnorm(0, mean = 0, sd = 1) # simulate the height of the prior at 0.

# Tada, a Savage Dickey Bayes Factor for the alternative hypothesis vs the null hypothesis
BF10 <- prior/posterior
BF10

# To test the evidence in favour of the null, you can swap nominator and denominator. Try yourself
BF01 <- ----
BF01


# Task 1: change the mean of the prior to 15
prior <- dnorm(0, mean = ---, sd = 1) 
prior/posterior

# How did the BF change?
# Are we still testing against the null hypothesis?


# Task 2: change the sd of the prior to 100
prior <- dnorm(0, mean = 0, sd = ---) 
prior/posterior

# How did the BF change? 
# The prior is more diffuse but the BF is (100 times) smaller.


