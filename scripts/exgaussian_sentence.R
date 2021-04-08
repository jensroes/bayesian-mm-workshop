# Load packages
library(tidyverse)
library(brms)


# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv")


# Check out the data
# Even on a log scale there is still a lot of positive skew
ggplot(data, aes(y = IKI, x = Lang, colour = transition_type,
                 group = transition_type)) +
  geom_jitter(size = .25, position = position_jitterdodge(jitter.width = .25)) +
  scale_y_log10() 


# Specify model (cell means)
# Note the family part
model <- bf(IKI ~ 1 + transition_type * Lang + (1 | SubNo), 
            beta ~ 1 + transition_type * Lang,
            family = exgaussian)


# Check out the priors for this model (some have defaults, others are flat
get_prior(model, data = data)


# Setup priors
prior <- set_prior("normal(250, 20)", class = "Intercept") +
         set_prior("normal(50, 10)", class = "sigma") +
         set_prior('normal(50, 10)', class = "b", dpar = "beta") +
         set_prior("normal(0, 20)", class = "b") 


# Run model
iter <- 4000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_exgaus <- brm(model, 
                     data = data, 
                     prior = prior, 
                     chains = chains, cores = cores,
                     iter = iter, warmup = warmup,
                     sample_prior = T)


# Check out the estimates 
fixef(fit_exgaus) 

conditional_effects(fit_exgaus)

# Save model
saveRDS(fit_exgaus, 
        file = "stanout/exgaussian_sentence.rda", 
        compress = "xz")


