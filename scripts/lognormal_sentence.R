# Load packages
library(tidyverse)
library(brms)


# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv")


# Descriptives
group_by(data, transition_type, Lang) %>%
  summarise(mean_iki = mean(IKI),
            sd_iki = sd(IKI))


# Specify model (cell means)
# Note the family part
model <- bf(IKI ~ 1 + transition_type * Lang + (1 | SubNo), family = lognormal)


# Check out the priors for this model (some have defaults, others are flat
get_prior(model, data = data)


# Setup priors
prior <-  set_prior("normal(6, 2)", class = "b") 
# this model has no intercepts and the slope (class b) are the cell means


# Run model
iter <- 4000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_lognormal <- brm(model, 
                     data = data, 
                     prior = prior, 
                     chains = chains, cores = cores,
                     iter = iter, warmup = warmup,
                     sample_prior = T)


# Check out the estimates cell means (compare to descriptives)
fixef(fit_lognormal) %>% exp() # the estimates are on a log rt scale; exp() is removing the log()


# Save model
saveRDS(fit_lognormal, 
        file = "stanout/lognormal_sentence.rda", 
        compress = "xz")


