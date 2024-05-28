# dependencies ------------------------------------------------------------
library(tidyverse)
library(patchwork)

# custom theme
custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 16),
        strip.text.y = element_text(face = "bold", family = "serif", size = 16), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14))

# period = 1 is effort
# period = {2,3,4} is VCM
# period > 5 is with punishment


# plot colors:
## orange = high (endowment 70)
## blue = low (endowment 30)
## treatment order: Unearned, Earned

# predictions
# Equal cont implies 25 each - this results in High earning more each period
# Equal proportion is 35 and 15 - this narrows the earnings difference
# Equal payoffs is 45 and 5 


# data and cleaning -------------------------------------------------------
df_all = readxl::read_excel("Normative Conflict and Punishment EQUAL UNEQUAL.xlsx")

df = df_all %>% 
  select(Period, Subject, Group, Session, Profit, Treatment, Type, Endowment, 
         Contribute, SanctionCost, Ave_Request, Ave_RequestLow, Ave_RequestHigh, 
         RequestLow, RequestHigh) %>% 
  rename_all(.funs = tolower) %>% 
  mutate(type_cat = case_when(
    treatment == 1 ~ 'Equal', 
    treatment != 1 & endowment == 30 ~ 'Low', 
    treatment != 1 & endowment == 70 ~ 'High'
  )) %>% 
  mutate(treatment_cat = case_when(
    treatment == 1 ~ 'Equal',
    treatment == 2 ~ 'Unearned', # unequal (high/low)
    treatment == 3 ~ 'Earned')) %>% # unequal (high/low)
  mutate(treatment_cat = forcats::fct_relevel(treatment_cat, 
                                              c('Equal', 
                                                'Unearned', 
                                                'Earned')))

# unique group
# treatment (2:3) + group (1:5) + session (1:11)
# end up with 15 uniquegroups for treatment = 3 and 14 unique groups for treatment = 2
df = df %>% 
  mutate(uniquegroup = treatment*1e3 + group*1e2 + session) %>% 
  mutate(uniquesubject = uniquegroup*1e2 + subject) %>% 
  arrange(uniquegroup, period, uniquesubject)

df = df %>% 
  mutate(endowment_type = ifelse(type_cat == 'Low', 1, 2))
