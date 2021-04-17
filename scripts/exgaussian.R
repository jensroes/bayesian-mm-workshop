# Load packages
library(tidyverse)
library(brms)

# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv") %>%
  select(SubNo, Lang, transition_type, IKI) %>%
  filter(IKI > 50, IKI < 5000)

# Specify model 
model <- bf(IKI ~ 1 + (1 | SubNo),  
            beta ~ 1 + (1 | SubNo), # decay parameter (exponential component)
            family = exgaussian())

# Check out the priors for this model (some have defaults, others are flat
#get_prior(model, data = data)

# Setup priors
prior <- set_prior("normal(250, 20)", class = "Intercept") +
         set_prior("normal(6, 2)", class = "Intercept", dpar = "beta")  # this is the decay rate (tau in slides) in log msecs

# Run model
iter <- 6000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_exgaus <- brm(model, 
                  data = data, 
                  prior = prior, 
                  chains = chains, cores = cores,
                  iter = iter, warmup = warmup,
                  sample_prior = TRUE,
                  control = list(adapt_delta = .99, 
                                 max_treedepth = 16),
                  seed = 365)


# Check out the estimates 
fixef(fit_exgaus) %>% round(2) # round numbers to make it more readable

# Save model
saveRDS(fit_exgaus, 
        file = "stanout/exgaussian_sentence.rda", 
        compress = "xz")
