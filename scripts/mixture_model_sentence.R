# Load packages
library(tidyverse)
library(brms)

# Load data (transitions for sentences only)
data <- read_csv("data/sentence_transitions.csv") %>%
  filter(SubNo %in% sample(unique(SubNo), 10))


# Check out the data
ggplot(data, aes(y = IKI, x = Lang, colour = transition_type,
                 group = transition_type)) +
  geom_jitter(size = .5, position = position_jitterdodge(jitter.width = .25)) +
  scale_y_log10() 


# Specify mixture model
mixture_of_lognormals = mixture(lognormal, nmix = 2, order = TRUE)


# Specify model 
model <- bf(IKI ~ 1 + transition_type * Lang + (1 | SubNo),
            theta2 ~ 1 + transition_type * Lang,
            family = mixture_of_lognormals)

# Check out the priors for this model (some have defaults, others are flat
get_prior(model, data = data)


# Setup priors
prior <-  set_prior("normal(5, 2)", class = "Intercept", dpar = "mu1") +
          set_prior("normal(9, 2)", class = "Intercept", dpar = "mu2") +
          set_prior("normal(0, 2)", class = "b", dpar = "mu1") +
          set_prior("normal(0, 4)", class = "b", dpar = "mu2") +
          set_prior("normal(0, .1)", dpar = "theta2") +
          set_prior("normal(0, .2)", class = "sigma1") + 
          set_prior("normal(1, .2)", class = "sigma2") 

# Run model
iter <- 4000 # Number of iterations
warmup <- iter / 2 # Warm-up samples
chains <- cores <- 3 # Number of chains (use one core of your machine per chain)

fit_mixturemodel <- brm(model, 
                     data = data, 
                     prior = prior, 
                     chains = chains, cores = cores,
                     iter = iter, warmup = warmup,
                     sample_prior = T)

#stancode(fit_mixturemodel)


bm <- fit_mixturemodel
plot(bm)

summary(bm)
eff = fixef(bm, summary = F) #this gives posterior samples just for fixed effects
psamps = posterior_samples(bm) #all psamps
stanplot(bm)
plot(bm)
pp_check(bm)
Df3 = cbind(Df2,ppm)

loo(fit1,fit2,bm)

ppm <- data.frame(pp_mixture(bm))
Df3 = cbind(Df2,ppm)
prpn_in_dist = Df3 %>% select(Lang,mu1 = Estimate.P.K...1...Y., mu2 = Estimate.P.K...2...Y.) %>% 
  gather(var,val,-Lang) %>% group_by(Lang,var) %>% 
  summarise(proportion = mean(val),
            N = n())

psamps_eff <- psamps %>% 
  mutate(mu1_EN = b_mu1_Intercept
         ,mu2_EN = b_mu2_Intercept
         ,mu1_ES = b_mu1_Intercept + b_mu1_LangES
         ,mu2_ES = b_mu2_Intercept + b_mu2_LangES) %>% 
  select(mu1_EN,mu2_EN,mu1_ES,mu2_ES)


MandCI <- function(v,Qlo = .025, Qhi = .95, rnd = 0){
  lo = quantile(v,Qlo)[[1]]
  hi = quantile(v,Qhi)[[1]]
  M = mean(v)
  paste0(round(M,rnd)," [",round(lo,rnd)
         ,",",round(hi,rnd),"]")
}

ests = prpn_in_dist

ests$M_CI = t(psamps_eff %>% summarise_all(MandCI))

kable(ests)


psamps_eff %>% gather(var,score) %>% 
  separate(var,c('distribution','language'), sep = '_') %>% 
  ggplot(aes(score,fill = language, linetype = distribution)) +
  geom_density(alpha=.5)






## compute the membership probabilities         
ppm <- pp_mixture(fit1)
str(ppm)

## extract point estimates for each observation
head(ppm[, 1, ])

## classify every observation according to 
## the most likely component
apply(ppm[, 1, ], 1, which.max)


# Check out the estimates cell means (compare to descriptives)
fixef(fit_mixturemodel) 


# Save model
saveRDS(fit_mixturemodel, 
        file = "stanout/mixture_model_sentence.rda", 
        compress = "xz")


