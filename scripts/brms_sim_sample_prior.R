# Load packages
library(tidyverse)
library(brms)
source("scripts/functions.R") # my function for simulating data


# Simulate data with an intercept of 300 and a slope of 15
data <- jens_data_machine(intercept = 300, slope = 15)


# Setup mixed-effects model
model <- bf(y ~ condition + (1 | participant_id))


# Look at priors: some have reasonable defaults, others are flat.
get_prior(model, data = data)


# Specify priors
prior <- set_prior("normal(500, 500)", class = "Intercept") +
        set_prior("normal(0, 100)", class = "b")


# Fit mixed-effects model 
# add sample_prior = TRUE to save the prior
fit_brm <- brm(formula = model, prior = prior, data = data, ---)


# Let's test some hypotheses:
# The logic for using the hypothesis() function is as follows.
# Use the name of the slope parameter in the model, type of comparison (<, >, =), 
# and value to compare to (0, 5, 10, etc).
# So if the name of the slope is "slopeb" and you want to test whether slopeb is 
# larger than 100 you state "slopeb > 100"


# Here we go...
# Remind yourself of the name of your slope parameter
names(fit_brm$fit)
# but ignore the prefix b_ for the below.


# 1. Now, test whether the slope is equal to zero (null hypothesis).
hypothesis(fit_brm, "---")
# The Bayes Factor is called Evid.Ratio (evidence ratio)


# 2. Test whether the slope is smaller than zero.
hypothesis(fit_brm, "---")


# 3. Test whether the slope is larger than zero.
hypothesis(fit_brm, "---")


# You can visualise how the height of the prior density at zero compares to 
# the height of the posterior density.
# Use any hypothesis from before:
h <- hypothesis(fit_brm, "---")

# Task: pass the object created with the hypothesis function to plot()
plot(---) 

# If you want to zoom in at zero:
# Task: pass the object created with the hypothesis function to plot()
plot(---, plot = F)[[1]] + coord_cartesian(xlim = c(-10, 10))


# Aside, Post.Prob in the hypothesis output is the posterior probability 
# which is calculated like this (see last session):
beta <- posterior_samples(fit_brm, pars = "b_conditionb") %>% pull() # extract beta
mean(beta > 0)
mean(beta < 0)


# Bonus: why is the probability interval (CI = credible interval in hypothesis)
# different for the null hypothesis test than for the other two tests?
# Compare to: fixef(fit_brm)
