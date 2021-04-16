# Load packages
library(brms)
library(tidyverse)
library(ggthemes)

# Load posteriors
files <- list.files("stanout", pattern = "brms_sim_with", full.names = T)

# Load models
posterior <- map(files, ~readRDS(.) %>% 
      posterior_summary(., "b_conditionb") %>% 
      as_tibble() %>%
      mutate(model = .x)) %>%
  bind_rows() %>%
  mutate(model = gsub("stanout/brms_sim_with_|.rda", "", model), 
         prior = recode(model, flat_prior = "Uniform(-Inf, Inf)",
         other_direction_prior = "N(-50, 10)",
         other_direction_weak_prior = "N(-50, 50)",
         weak_prior = "N(0, 50)",
         zero_prior = "N(0, 10)")) %>% select(-model)

post <- posterior %>% expand_grid(vars(prior, Estimate, `Est.Error`), x) %>%
  mutate(distribution = paste0("dnorm(",x, ", ", Estimate, ", ", `Est.Error`, ")")) %>%
  select(prior, distribution, x) %>%
  rowwise() %>%
  mutate(y = eval(parse(text = distribution))) %>%
  select(-distribution) %>%
  ungroup() %>% unique()

# Look up the coefficients (without prior involved)  
data <- jens_data_machine(intercept = 300, slope = 15)
m <- lme4::lmer(y ~ condition + (1|participant_id), data)
coefs <- coef(summary(m))[2,1:2]
round(coefs,2)

x <- seq(-100, 100, 1)
prior_and_data <- tibble("data" = dnorm(x, 15.59, 8.14),
       dnorm(x, 0, 50),
       dunif(x, -Inf, Inf),
       dnorm(x, 0, 10),
       dnorm(x, -50, 10),
       dnorm(x, -50, 50),
       x) %>% pivot_longer(-x, values_to = "y", names_to = "prior") %>%
  mutate(prior = gsub("dnorm", "N", prior),
         prior = gsub("x, ", "", prior),
         prior = gsub("dunif", "Uniform", prior)) 

all_dists <- bind_rows("prior" = prior_and_data, "posterior" = post, .id = "group") %>%
  mutate(group = if_else(prior == "data", "data", group))

no_data <- all_dists %>% arrange(prior, group) %>% filter(group != "data")
data <- all_dists %>% arrange(prior, group) %>% filter(group == "data") 

all_dists <- data %>% 
  select(-prior) %>%
  expand_grid(prior = unique(post$prior)) %>%
  bind_rows(no_data)


write_csv(all_dists, "data/prior_simulation.csv")
