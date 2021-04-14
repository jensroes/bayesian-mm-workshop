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
#*intercept: a normal distribution with a mean of 500 and a variance of 500
#*slope (b): a normal distribution with a mean of 0, and a variance of 100
#*These priors are weakly informative.
prior <- set_prior("normal(---, ---)", class = "Intercept") +
         set_prior("---(---, ---)", class = "b")


# Fit mixed-effects model 
fit_brm <- brm(formula = model, prior = prior, data = data)


# Evaluate model coefficients using the name of the model
fixef(---)


# Save posterior (so you don't have to run the model again)
saveRDS(object = fit_brm, 
        file = "stanout/brms_sim_with_prior.rda",
        compress = "xz")

