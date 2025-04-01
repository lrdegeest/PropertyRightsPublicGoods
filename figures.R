# dependencies and data ---------------------------------------------------
source("data.R")


# figure 1 ----------------------------------------------------------------

cont_p1 = df %>% 
  filter(period > 4 & treatment != 1) %>% 
  ggplot(aes(x = type_cat, y = contribute, fill = treatment_cat)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.25) +
  scale_fill_manual(values = c('orange', 'blue'), name = "") +
  geom_hline(yintercept = c(15, 35), linetype = c('dotted', 'dashed')) + # equal proportion norm
  labs(x = "", y = 'Contribution') + 
  custom_theme + 
  theme(legend.position = c(0.85, 0.85))

# time series
cont_p2 = df %>%
  filter(period > 4 & treatment != 1) %>% # effort task
  group_by(period, type_cat, treatment_cat) %>% 
  ggplot(aes(x = period, y = contribute, color = treatment_cat, group = treatment_cat)) + 
  #geom_vline(xintercept = 4.5, linetype = 'dotted') + 
  geom_hline(yintercept = 15, linetype = 'dotted', color = 'black') + # equal proportion norm
  geom_hline(yintercept = 35, linetype = 'dashed', color = 'black') + 
  stat_summary(fun = 'mean', geom = 'point', position = 'dodge', alpha = 0.5) + 
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1) + 
  stat_summary(fun = 'mean', geom = 'line') +  
  facet_wrap(~type_cat) + 
  scale_color_manual(values = c('orange', "blue"), name = "") + 
  ylim(c(0,70)) + 
  labs(x = 'Period', y = 'Average Contribution') + 
  custom_theme + 
  theme(legend.position=c(0.85,0.85))


cont_p1 + cont_p2 + 
  plot_annotation(tag_levels = 'A')  
  


# figure 2 ----------------------------------------------------------------
df_target = haven::read_dta("targeting_data.dta")
df_target = df_target %>% 
  mutate(treatment_cat = case_when(
    treatment == 1 ~ 'Equal',
    treatment == 2 ~ 'Unearned', # unequal (high/low)
    treatment == 3 ~ 'Earned')) %>% # unequal (high/low)
  mutate(treatment_cat = forcats::fct_relevel(treatment_cat, 
                                              c('Equal', 
                                                'Unearned', 
                                                'Earned')))
p1 = df_target %>% 
  filter(treatment != 1) %>% 
  filter(target_endowment == 70) %>%
  mutate(cont_binned = cut(target_contribution,
                           breaks = seq(0, 70, by = 5),
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = paste0("[", seq(0, 65, by = 5), ",", seq(5, 70, by = 5), ")"))) %>%
  group_by(treatment_cat, cont_binned) %>%
  summarise(
    s = mean(target_sanction),
    se = sd(target_sanction) / sqrt(n())  # Calculate standard error
  ) %>%
  ggplot(aes(x = cont_binned, y = s, fill = treatment_cat)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin = s - se, ymax = s + se), width = 0.2, position = position_dodge(width = 0.9)) +  # Add error bars
  labs(x = 'Binned Contributions', y = 'Average Punishment Received', subtitle = 'High Types') + 
  scale_fill_manual(values = c('orange', "blue"), name = "") + 
  custom_theme 

p2 = df %>%
  filter(period > 4) %>%
  filter(type_cat == "Low") %>%
  filter(treatment_cat != "Equal") %>%
  mutate(cont_binned = cut(contribute,
                           breaks = seq(0, 30, by = 5),
                           include.lowest = TRUE,
                           right = FALSE,
                           labels = paste0("[", seq(0, 25, by = 5), ",", seq(5, 30, by = 5), ")"))) %>%
  group_by(treatment_cat, cont_binned) %>%
  summarise(
    s = mean(sanctioncost),
    se = sd(sanctioncost) / sqrt(n())  # Calculate standard error
  ) %>%
  ggplot(aes(x = cont_binned, y = s, fill = treatment_cat)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin = s - se, ymax = s + se), width = 0.2, position = position_dodge(width = 0.9)) +  # Add error bars
  labs(x = 'Binned Contributions', y = 'Average Punishment Received', subtitle = 'Low Types') + 
  scale_fill_manual(values = c('orange', "blue"), name = "") + 
  custom_theme 


p1 + p2 + 
  plot_layout(guides = 'collect')
