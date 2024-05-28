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
# not used in manuscript

pun_p1 = df %>%
  filter(period > 1 & treatment != 1) %>% # effort task
  mutate(did_pun = ifelse(sanctioncost > 0, 1, 0)) %>% 
  group_by(type_cat, treatment_cat) %>% 
  ggplot(aes(x = treatment_cat, y = did_pun, fill = type_cat)) + 
  stat_summary(fun = 'mean', geom = 'bar', position = 'dodge', alpha = 0.75) + 
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.1, position = position_dodge(width=0.9)) + 
  scale_fill_manual(values = c('orange', 'blue'), name = "") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "", y = 'Punishment Received Rate') + 
  custom_theme + 
  theme(legend.position = c(0.85, 0.85))


pun_p2 = df %>% 
  filter(period > 4 & treatment != 1 & sanctioncost > 0) %>% 
  ggplot(aes(x = treatment_cat, y = sanctioncost, fill = type_cat)) +
  geom_boxplot(alpha = 0.75, outlier.alpha = 0.25) +
  scale_fill_manual(values = c('orange', 'blue'), name = "") +
  labs(x = "", y = 'Punishment Received',
       caption = 'Only non-zero punishment') + 
  custom_theme + 
  theme(legend.position = c(0.85, 0.85))

pun_p1 + pun_p2  + 
  plot_annotation(tag_levels = 'A')
