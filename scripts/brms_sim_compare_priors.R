# Load packages
library(tidyverse)
library(brms)
source("scripts/functions.R") # my function for simulating data

# Simulate data with an intercept of 300 and a slope of 15
data <- jens_data_machine(intercept = 300, slope = 15)

# Specify model
model <- bf(y ~ condition + (1 | participant_id))

## 1. weak (regulating) prior
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(0, 50)", class = "b")

fit_brm_weak <- brm(formula = model, prior = prior, data = data)

saveRDS(object = fit_brm_weak, 
        file = "stanout/brms_sim_with_weak_prior.rda",
        compress = "xz")

## 2. flat prior
prior <- set_prior("normal(500, 250)", class = "Intercept") 

fit_brm_flat <- brm(formula = model, prior = prior, data = data)

saveRDS(object = fit_brm_flat, 
        file = "stanout/brms_sim_with_flat_prior.rda",
        compress = "xz")


## 3. informative on zero
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(0, 10)", class = "b")

fit_brm_zero <- brm(formula = model, prior = prior, data = data)

saveRDS(object = fit_brm_zero, 
        file = "stanout/brms_sim_with_zero_prior.rda",
        compress = "xz")


# 4. other direction
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(-50, 10)", class = "b")

fit_brm_other_dir <- brm(formula = model, prior = prior, data = data, 
                       cores = 3, chains = 3)

fixef(fit_brm_other_dir)

saveRDS(object = fit_brm_other_dir, 
        file = "stanout/brms_sim_with_other_direction_prior.rda",
        compress = "xz")


# 5. other direction weak
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(-50, 50)", class = "b")

fit_brm_other_dir_flat <- brm(formula = model, prior = prior, data = data, 
                         cores = 3, chains = 3)

fixef(fit_brm_other_dir_flat)

saveRDS(object = fit_brm_other_dir_flat, 
        file = "stanout/brms_sim_with_other_direction_weak_prior.rda",
        compress = "xz")

