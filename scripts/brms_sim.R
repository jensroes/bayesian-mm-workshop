# Load packages
library(tidyverse)
library(brms)
source("scripts/functions.R") # my function for simulating data

# Simulate data with an intercept of 300 and a slope of 15
data <- jens_data_machine(intercept = ---, slope = ---)


# Check out the data
glimpse(---)


# Plot the data: outcome variable on y and condition on x
ggplot(data = data, aes(y = ---, x = ---)) +
  geom_boxplot()


# Fit mixed-effects model with condition as fixed effect and 
# participant as random intercepts term:
fit_brm <- brm(y ~ 1 + --- + (1 | ---), data = data)


# Evaluate model coefficients using the name of the model
fixef(---)


# Save posterior (so you don't have to run the model again)
saveRDS(object = fit_brm, 
        file = "stanout/brms_sim.rda",
        compress = "xz")

