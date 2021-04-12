library(brms)

# Load model
fit_exgaus <- readRDS(file = "stanout/exgaussian_sentence.rda")

# Inspect fit
loo(fit_exgaus)

# This will take some time ...
kfold_exgaus <- kfold(fit_exgaus, K = 10)

# Save result
saveRDS(kfold_exgaus, 
        file = "stanout/exgaussian_sentence_kfold_loo.rda", 
        compress = "xz")
