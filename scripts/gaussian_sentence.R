# Load packages
library(tidyverse)
library(brms)


# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv") %>%
  select(SubNo, Lang, transition_type, IKI) %>%
  filter(IKI > 50, IKI < 5000)


# Specify model
model <- bf(IKI ~ 1 + (1 | SubNo))


# Check out the priors for this model (some have defaults, others are flat
get_prior(model, data = data)


# Setup priors
prior <- set_prior("normal(400, 200)", class = "Intercept") 


# Run model
iter <- 6000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_norm <- brm(model, 
                data = data, 
                prior = prior, 
                chains = chains, cores = cores,
                iter = iter, warmup = warmup,
                sample_prior = TRUE,
                seed = 365)


# Check out the estimates 
fixef(fit_norm) %>% round(2) # round numbers to make it more readable


# Save model
saveRDS(fit_norm, 
        file = "stanout/gaussian_sentence.rda", 
        compress = "xz")
