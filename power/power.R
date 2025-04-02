# data and dependencies ---------------------------------------------------
source("data.R") # aggregate (non-targeting) experiment data
library(lmerTest)
library(clubSandwich)
library(progress)


# simulate linear model with random effects -------------------------------
simulate_effect = function(.data, model){
  
  data_copy = .data
  
  # extract stuff from fitted RE model
  # intercept and treatment indicator
  fixed_effects = unname(fixef(model)) 
  
  #  random effects variance
  random_effects_var = as.numeric(VarCorr(model)$uniquesubject)
  
  # residual variance
  residual_var = attr(VarCorr(model), "sc")^2 
  
  # simulate random effects
  n_subjects = length(unique(data_copy$uniquesubject)) 
  random_effects = rnorm(n_subjects, mean = 0, sd = sqrt(random_effects_var))
  n_obs = nrow(data_copy) 
  
  # simulate residuals
  residuals = rnorm(n_obs, mean = 0, sd = sqrt(residual_var))
  
  # simulate contribution from estimated model
  data_copy$uniquesubject_id = rep(1:n_subjects, each = n_obs/n_subjects)
  data_copy$random_effect = random_effects[data_copy$uniquesubject_id]
  
  data_copy$simulated_contribute =
    fixed_effects[1] + 
    fixed_effects[2] * data_copy$treatment +
    data_copy$random_effect +
    residuals
  
  # estimate treatment effect on the simulated model
  sim_model = lmer(simulated_contribute  ~ treatment_cat + (1 | uniquesubject), data = data_copy) 
  
  # cluster the errors and get a p-value
  res = coef_test(sim_model, vcov = "CR0", cluster = .data$uniquegroup, coefs = 'treatment_catEarned')
  
  return(res)
  
}

power_linear_model = function(.data, model, alpha = 0.05, n_sims = 10000) {
  pb = progress_bar$new(
    format = "Simulating [:bar] :percent eta: :eta",
    total = n_sims, clear = FALSE, width = 60
  )
  
  sim_results = vector("list", n_sims)
  
  for (i in seq_len(n_sims)) {
    sim_results[[i]] = simulate_effect(.data = .data, model = model)
    pb$tick()
  }
  
  sim_df = bind_rows(sim_results)
  return(list("mean_effect" = mean(sim_df$beta), "power" = mean(sim_df$p_Satt < alpha)))
}


# compare RE estimations to stata --------------------------------------------

# just high types
high_df = df %>% 
  filter(treatment !=1 & period > 4 & endowment == 70) %>% 
  arrange(uniquegroup, uniquesubject, period)

# fit 
m_high = lmerTest::lmer(contribute ~ treatment_cat + (1 | uniquesubject), data = high_df) 
# results line up with stata
coef_test(m_high, vcov = "CR0", cluster = high_df$uniquegroup, coefs = 'treatment_catEarned')

# low types
low_df = df %>% 
  filter(treatment !=1 & period > 4 & endowment == 30) 
# fit
m_low = lmerTest::lmer(contribute ~ treatment_cat + (1 | uniquesubject), data = low_df) 
# results line up with stata
coef_test(m_low, vcov = "CR0", cluster = low_df$uniquegroup, coefs = 'treatment_catEarned')


# simulate power from linear model ----------------------------------------
high_sim_linear = power_linear_model(high_df, m_high, n_sims = 1e4)
low_sim_linear = power_linear_model(low_df, m_low, n_sims = 1e4)
