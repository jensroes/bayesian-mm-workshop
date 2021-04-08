
jens_data_machine <- function(intercept = 250, slope = 25, 
                              intercept_sd = 5, slope_sd = 5, 
                              error = 100, participants = 30, trials_per_condition = 10){
  set.seed(365)
  # Simulate data
  participants <- participants # number of subjects
  trials <- trials_per_condition # number of observations per subject per condition
  conditions <- 2
  
  # Population parameters
  intercept <- intercept
  intercept_sd <- intercept_sd
  slope <- slope
  slope_sd <- slope_sd
  error <- error # trial-by-trial error
  
  intercept <- rnorm(participants, intercept, intercept_sd)
  slope <- rnorm(participants, slope, slope_sd)
  
  data <- tibble(participant_id = rep(1:participants, each = trials*conditions),
                 trial_id = rep(1:trials, conditions*participants),
                 condition = rep(letters[1:conditions], trials*participants),
                 y = -999) 
  
  # iterate over subject to generate data for each one
  data_sim <- data %>% group_by(participant_id, condition) %>%
    mutate(y = if_else(condition == letters[1],
                           rnorm(trials, intercept[participant_id], error), y),
               y = if_else(condition == letters[2],
                           rnorm(trials, intercept[participant_id] + slope[participant_id], error), y))
#  print(paste("There you go! Data for", participants, "participants with each contributing", trials, "trials to 2 conditions.")) 
  return(data_sim)
  #fit <- lmer(y ~ condition + (1|participant_id), data = data_sim)
  #summary(fit)
  #sigma(fit)
}