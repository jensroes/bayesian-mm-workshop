# Load packages
library(tidyverse)
library(brms)
source("scripts/functions.R") # my function for simulating data

# Simulate data with an intercept of 300 and a slope of 15
data <- jens_data_machine(intercept = ---, slope = ---)


# Check out the data
glimpse(data)


# Plot the data: outcome variable on y and `condition` on x
ggplot(data = data, aes(y = ---, x = ---)) +
  geom_boxplot()


# Setup mixed-effects model with condition as fixed effect :
model <- bf(y ~ 1 + --- + (1 | participant_id))
# specifying model outside of brms works for lmer too.
# bf = brmsformula

# Look at priors: some have reasonable defaults, others are flat.
get_prior(model, data = data)


# Specify priors. We want for 
#*intercept: a normal distribution with a mean of 500 and a variance of 250
#*slope (b): a normal distribution with a mean of 0, and a variance of 100
#*These priors are weakly informative.
prior <- set_prior("normal(---, ---)", class = "Intercept") +
         set_prior("---(---, ---)", class = "b")


# Fit mixed-effects model 
fit_brm <- brm(formula = model, prior = prior, data = data,
               cores = 3, chains = 3) # to speed things up, lets run 3 chains in parallel


# Evaluate model coefficients using the name of the model
fixef(---)


# Save posterior (so you don't have to run the model again)
saveRDS(object = fit_brm, 
        file = "stanout/brms_sim_with_prior.rda",
        compress = "xz")



# weak
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(0, 100)", class = "b")

fit_brm_weak <- brm(formula = model, prior = prior, data = data)

# flat
prior <- set_prior("normal(500, 250)", class = "Intercept") 

fit_brm_flat <- brm(formula = model, prior = prior, data = data)

# zero
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(0, 1)", class = "b")

fit_brm_zero <- brm(formula = model, prior = prior, data = data)

# against hypothesis
prior <- set_prior("normal(500, 250)", class = "Intercept") +
  set_prior("normal(-500, 10)", class = "b")

fit_brm_against <- brm(formula = model, prior = prior, data = data, 
                       cores = 3, chains = 3)

fixef(fit_brm_weak)
fixef(fit_brm_flat)
fixef(fit_brm_zero)
fixef(fit_brm_against)

beta <- posterior_samples(fit_brm_against, pars = "b_conditionb") %>% pull()
hist(beta)



