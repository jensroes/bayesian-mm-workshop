library(brms)
library(tidyverse)

# Model names
dir("stanout")

# Load mixture model
mixturemodel <- readRDS("stanout/mixture_model_sentence.rda")

# Check out the names of the coefficients:
names(mixturemodel$fit)

# Select the population estimates
# Task: there is one piece of information missing. We need the population 
# estimate of the mixing proportion. Add the correct name:
coefs <- c("b_mu1_Intercept", "b_mu2_Intercept", "---", "sigma1", "sigma2")

# Summary of coefficients
# Task: `pars` needs to know which parameter to summarise. Their names are 
# stored in `coefs`
posterior_summary(mixturemodel, pars = ---, fixed = T) %>%
  round(2)

# Posterior parameter values
# Task: we want to plot the posterior distributions as histograms, 
# called `hist`.
mcmc_plot(mixturemodel, type = "---", pars = coefs, fixed = T)


# Look at mixing proportion theta by participant
# Extract parameter value estimates
# Task: Nothing to complete here but make sure you can make sense of the 
# plot below.
ppt_vars <- posterior_summary(mixturemodel, pars = "r_SubNo__theta2") %>%
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

# Short cut (but unordered)
mcmc_plot(mixturemodel, pars = "r_SubNo__theta2")
         