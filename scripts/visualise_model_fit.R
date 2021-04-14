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
# Task: complete the "---" for the mixture model.
mixturemodel <- readRDS("---")


# Simulate n predictions from each model and compare
# their density against the data.

# First create 100 simulations for each model, create a plot and store it in a 
# variable (e.g. `p_gaussian`).

nsamples <- 100 # number of simulations

p_gaussian <- pp_check(gaussian, nsamples = nsamples) +
  labs(title = "Gaussian") 

# Check out `p_gaussian`
p_gaussian # looks terrible, eh?!

# Create plots for the other four models.
p_skewnormal <- pp_check(skewnormal, nsamples = nsamples) +
  labs(title = "skew-Normal") 

p_lognormal <- pp_check(lognormal, nsamples = nsamples) +
  labs(title = "log-Normal") 

p_exgaussian <- pp_check(exgaussian, nsamples = nsamples) +
  labs(title = "ex-Gaussian") 

# Task: Complete this one for the mixture model and assign the output to p_mixture.
--- <- pp_check(---, nsamples = ---) +
  labs(title = "Mixture model") 


# Combine all 5 plots
# Task: one plot is missing(indicated by the ---s); add the correct one.
all_plots <-  p_gaussian + p_skewnormal  + --- + p_exgaussian + p_mixture  
  coord_cartesian(xlim = c(0, 3000)) & theme_bw() &
  theme(legend.position = "bottom",
        legend.justification = "left")


# Do some magic to just have one legend instead of 5.
all_plots + plot_layout(guides = "collect", nrow = 2) 
# Which model, do you think, does best at predicting the observed data?


# Bonus: change the `nsamples` variable from 100 to 10 (and create plots) and 
# then to 1000 (and create plots) to see that the simulation is not a by product of 
# random sampling.


# Save plot (for slides)
ggsave("slides/pics/modelcomps.png", width = 8, height = 6)

