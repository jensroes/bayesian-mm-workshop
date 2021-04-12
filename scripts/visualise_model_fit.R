library(brms)
library(tidyverse)
library(patchwork) # for combining plots

# Model names
dir("stanout")

# Load models

# Gaussian (Normal)
gaussian <- readRDS("stanout/gaussian_sentence.rda")

# Log-Normal
lognormal <- readRDS("stanout/lognormal_sentence.rda")

# Skew-Normal
skewnormal <- readRDS("stanout/skewnormal_sentence.rda")

# ex-Gaussian
exgaussian <- readRDS("stanout/exgaussian_sentence.rda")

# Mixture model
mixturemodel <- readRDS("stanout/mixture_model_sentence.rda")


# Simulate n predictions from each model and compare
# their density against the data
nsamples <- 100 # number of simulations

p_mixture <- pp_check(mixturemodel, nsamples = nsamples) +
  labs(subtitle = "Mixture mode") +
  scale_x_log10() 

p_gaussian <- pp_check(gaussian, nsamples = nsamples) +
  labs(subtitle = "Gaussian") +
  scale_x_log10()

p_lognormal <- pp_check(lognormal, nsamples = nsamples) +
  labs(subtitle = "log-Normal") +
  scale_x_log10()

p_skewnormal <- pp_check(skewnormal, nsamples = nsamples) +
  labs(subtitle = "skew-Normal") +
  scale_x_log10()

p_exgaussian <- pp_check(exgaussian, nsamples = nsamples) +
  labs(subtitle = "ex-Gaussian") +
  scale_x_log10()

# Combine all 5 plots
( p_gaussian | p_lognormal | p_skewnormal ) / ( p_exgaussian | p_mixture )

# Which model, do you think, has the best fit?
