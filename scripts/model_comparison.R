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
# Task: complete this line correctly for the skew-normal model.
skewnormal --- ---

# ex-Gaussian
exgaussian <- readRDS("stanout/exgaussian_sentence.rda")

# Mixture model
mixturemodel <- readRDS("stanout/mixture_model_sentence.rda")


# You can use loo to compare two models:
# Task: Compare two models (your choice) by adding the names as defined above (e.g. gaussian).
loo(---, ---)
# These are the fit statistics. On the bottom is the *model comparison*. 
# elpd_diff is the difference between your models with the better model in the first row
# and the worse model on the bottom row.


# Compare all models using leave-one-out cross validation.
# Task: there is one model missing in the call below; add the right one and replace "---".
model_comparisons <- loo(gaussian, 
                         lognormal, 
                         ---, 
                         exgaussian, 
                         mixturemodel)

# Fit statistic for skew-Normal and ex-Gaussian are probably too optimisitc.
# There is no need to refit the models though as their performance is worse than 
# for the mixture model anyway.

# Extract differences
model_comparisons$diffs
# Task: Which model is the best?

# You can also extract the other stats
model_comparisons$diff %>% as.data.frame() %>% round()

# and indeed look at the fit statistics of every model (but this format is a 
# little clumsy).

# Save comparison results
saveRDS(model_comparisons, "stanout/model_comparison.rda")
