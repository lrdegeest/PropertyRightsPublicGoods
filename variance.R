# dependencies and data ---------------------------------------------------
library(lmerTest) # runs lme4::lmer with added bells & whistles
source("data.R")

df = df %>% filter(treatment != 1)


treatment_results_list = vector(mode = 'list', length = length(unique(df$treatment)))
for(i in seq_along(treatment_results_list)){

    df_filter = df %>% 
      filter(period > 4 & period <= 20 & treatment != 1 & endowment_type == i)
    
    m1 = lmerTest::lmer(contribute ~ treatment + (1 | uniquegroup) + (1 | uniquesubject), data = df_filter)
    
    df_filter = df_filter %>% 
      mutate(residual_contribute = abs(residuals(m1)))
    
    m2 = lmerTest::lmer(residual_contribute ~ treatment + (1 | uniquesubject), data = df_filter)
    
    treatment_results_list[[i]] = m2 %>% 
      car::Anova(mod = ., type = 3, test.statistic = 'Chisq') %>% 
      broom::tidy() %>% 
      filter(term == 'treatment') %>% 
      mutate(type = i,
             type_cat = ifelse(i == 1, 'Low', 'High'))
    
    rm(df_filter)

}
treatment_results_df = dplyr::bind_rows(treatment_results_list)
treatment_results_df


