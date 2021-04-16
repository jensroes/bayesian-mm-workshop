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

# By model comparisons of the leave-one-out information criteria
model_comparisons$ic_diffs__

# and indeed look at the fit statistics of every model
model_comparisons

# Save comparisons so you don't need to run it again.
saveRDS(model_comparisons, "stanout/model_comparison.rda")

# You might prefer a csv table you can open in excel with all the important bits.
# Here you go:
model_comparisons_all <- model_comparisons$diff %>% as.data.frame()
write_csv(model_comparisons_all, "stanout/model_comparison.csv")


# Lastly if you're wondering about "statistical significance" (which, btw, doesn't exist) between the 
# best and second best model you can think of their difference (SE) as being normal distributed.
# We can simulate n values from a normal distribution and calculate the proportion of values
# that is on the other side of 0.
n <- 1e5
diff <- rnorm(n, -626.3, 56.3) # simulate values
# These two values might be slightly different for you but should be elpd_diff (se_diff)
# of the second best model.
hist(diff) # look at the simulations
mean(diff > 0) # proportion of values that have a difference of larger than 0.

# Compare second best to third best model.
mean(diff < -736.8)

# Try and compare the second best model to the fourth best model.
mean(diff --- ---)

# Repeat the above and compare the fourth and the fifth model in the comparison table:
diff <- rnorm(n, ---, ---)
hist(---)
mean(diff --- ---)

