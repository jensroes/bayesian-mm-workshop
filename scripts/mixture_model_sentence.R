# Load packages
library(tidyverse)
library(brms)

# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv") %>%
  select(SubNo, Lang, transition_type, IKI) %>%
  filter(IKI > 50, IKI < 5000)


# Specify mixture model
mixture_of_lognormals <- mixture(lognormal(), lognormal(), order = TRUE)


# Specify model 
model <- bf(IKI ~ 1 + (1 | SubNo),
            theta2 ~ 1 + (1 | SubNo),
            family = mixture_of_lognormals)


# Check out the priors for this model (some have defaults, others are flat
#get_prior(model, data = data)


# Setup priors
prior <-  set_prior("normal(4, 1)", class = "Intercept", dpar = "mu1") +
          set_prior("normal(6, 1)", class = "Intercept", dpar = "mu2") +
          set_prior("beta(2, 2)", class = "Intercept", dpar = "theta2") 


# Run model
iter <- 6000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_mixturemodel <- brm(model, 
                     data = data, 
                     prior = prior, 
                     chains = chains, cores = cores,
                     iter = iter, warmup = warmup,
                     sample_prior = TRUE,
                     control = list(adapt_delta = .99, 
                                    max_treedepth = 16),
                     seed = 365)


# Check out the estimates 
fixef(fit_mixturemodel) %>% round(2)

# Save model
saveRDS(fit_mixturemodel, 
        file = "stanout/mixture_model_sentence.rda", 
        compress = "xz")
