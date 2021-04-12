library(brms)
library(tidyverse)

# Model names
dir("stanout")

# Load mixture model
fit_mixturemodel <- readRDS("stanout/mixture_model_sentence.rda")

# Check out the names of the coefficients
names(fit_mixturemodel$fit)

# Select the population estimates
coefs <- c("b_mu1_Intercept", "b_mu2_Intercept", "b_theta2_Intercept", "sigma1", "sigma2")

# Summary of coefficients
posterior_summary(fit_mixturemodel, pars = coefs) %>%
  round(2)

# Posterior parameter values
mcmc_plot(fit_mixturemodel, type = "hist", pars = coefs, fixed = T)

# Match with data
pp_check(fit_mixturemodel, nsamples = 100) +
  scale_x_log10() # convert to log scale for visibility


# Look at mixing proportion theta by participant
# Extract parameter value estimates
ppt_vars <- posterior_summary(fit_mixturemodel, pars = "r_SubNo__theta2") %>%
  as.data.frame() %>%
  rownames_to_column("SubNo") %>%
  mutate(SubNo = str_match(SubNo, "S-\\s*(.*?)\\s*[,]")[,1],
         SubNo = gsub(",", "", SubNo)) %>%
  as_tibble()

# Print the calculated values
ppt_vars

# Plot the by-ppt values
# Values above 0: ppt shows more long values than average
# Values below 0: ppt shows less long values than average
ggplot(ppt_vars, aes(y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`, 
                     x = reorder(SubNo, Estimate))) +
  geom_pointrange() +
  coord_flip() + theme_bw() +
  labs(y = "Difference from population level mixing proportion in SDs",
       x = "Participant id")

# Short cut
mcmc_plot(fit_mixturemodel, pars = "r_SubNo__theta2")
         