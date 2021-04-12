# Load packages
library(brms)
library(tidyverse)

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

# Compare models using leave-one-out cross validation
model_comparisons <- loo(gaussian, 
                         lognormal, 
                         skewnormal, 
                         exgaussian, 
                         mixturemodel)

# Fit statistic for skew-Normal and ex-Gaussian are probably over optimisitc
# but there is no need to refit the models as their performance is worse than 
# for the mixture model anyway.

# Extract differences
model_comparisons$diffs

# Show other stats
model_comparisons$diff %>% as.data.frame() %>% round()

# Save comparison results
saveRDS(model_comparisons, "stanout/model_comparison.rda")
