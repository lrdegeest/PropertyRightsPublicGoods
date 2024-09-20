# about -------------------------------------------------------------------
# this file reshapes the targeting data so to estimate contribution norms
# produces "targeting_data.dta"
# modified from https://github.com/lrdegeest/NormEnforcement/blob/master/data/format_norms_data/reshape_punishment_data.R
# author: @lrdegeest

# dependencies ------------------------------------------------------------
library(tidyverse)

# data --------------------------------------------------------------------
df_all = readxl::read_excel("Normative Conflict and Punishment EQUAL UNEQUAL.xlsx")


# cleaning ----------------------------------------------------------------
df = df_all %>% 
  filter(Period > 4) %>% 
  select(Period, Subject, Group, Session, Profit, Treatment, Type, Endowment, 
         Contribute, SanctionCost, AdminCosts,
         IndNum,
         starts_with("otherscontribution"),
         starts_with("othersendowment"),
         starts_with("sanction")) %>%  
  rename_all(.funs = tolower) %>% 
  # unique group
  # treatment (2:3) + group (1:5) + session (1:11)
  # end up with 15 uniquegroups for treatment = 3 and 14 unique groups for treatment = 2
  mutate(uniquegroup = treatment*1e3 + group*1e2 + session) %>% 
  mutate(uniquesubject = uniquegroup*1e2 + subject) %>% 
  relocate(uniquegroup, .before = everything()) %>% 
  relocate(uniquesubject, .after = uniquegroup) %>% 
  arrange(uniquegroup, uniquesubject, period) %>% 
  select(-c(subject, group, session))

n = 4 # group size
df <- df[with(df, order(uniquegroup,indnum,period)),] # this sort is SUPER important
colnames(df) = gsub("others", "", colnames(df))


# functions ---------------------------------------------------------------
do_recast <- function(targets_data, sender_data=NULL) {
  "
  arguments:  
    - targets_data    target dataframe (contributions, endowments or punishment). Type: wide panel
    - sender_data     sender dataframe, indexed by indnum (session, date, group, endow, cont, sanctioncost). Type: wide panel
  
  returns:
    - data_long       reshaped target dataframe. Type: long panel
  "
  targets_data$uniquegroup <- sender_data$uniquegroup
  targets_data$period <- sender_data$period
  targets_data$indnum <- sender_data$indnum
  data_long <- reshape2::melt(targets_data, id=c("uniquegroup", "indnum", "period"))
  data_long <- data_long[order(data_long$period),]
  colname <- names(targets_data)[1]
  colname <- gsub("([[:punct:]][[:digit:]])", "", colname)
  colname <- gsub("]", "", colname)
  colname <- gsub("[0-9].*$", "", colname)
  colname <- paste0("target", "_", colname)
  names(data_long)[names(data_long)=="value"] <- colname
  data_long$variable <- NULL
  data_long <- data_long[with(data_long, order(uniquegroup,indnum, period)),] # same sort as original data
  rownames(data_long) <- NULL
  return(data_long)
}

get_sanctions_indnum <- function(data, id) { # "data" is full imported data ("df")
  "
  arguments:  
    - data    full imported data with all indnums
    - indnum  the sender's ID (indnum)

  returns:
    - df.sender.all   complete reshaped dataframe for sender and all targets 

  example:
    d1 <- get_sanctions_indnum(df,1) # get sanctions data for all indnum==1
  "
  df.indnum <- subset(df, indnum == id) # subset for the indnum
  df.sender <- df.indnum[,1:11] # sender columns
  df.targets <- df.indnum[,12:ncol(df.indnum)] #targets columns
  df.targets <- subset(df.targets, select = ifelse(grepl(paste0("_",id), names(df.targets)) + grepl(paste0("sanction",id), names(df.targets)) == 1, TRUE, FALSE)) # select only the indnum columns
  target_list <- list("endow" = df.targets[,1:3], "cont" = df.targets[,4:6], "punish"= df.targets[,7:9])
  target_list_recast <- lapply(target_list, do_recast, sender_data = df.sender) # reshape endow, cont and punish data
  df.sender <- df.sender[rep(1:nrow(df.sender),each=(n-1)),] # extra rows for sender before merge; must come AFTER lapply
  df.sender.all <- cbind(df.sender, target_list_recast[[1]], target_list_recast[[2]], target_list_recast[[3]]) # bind
  df.sender.all <- df.sender.all[, !duplicated(colnames(df.sender.all))] # drop duplicate cols
  df.sender.all$target_rank <- rep(seq(1:3),length(unique(df.sender.all$period))) # add the target rank, always same order (1,2,3)
  return(df.sender.all)
}

get_all_sanctions_data <- function(data) {
  "
  arguments:  
  - data    full imported data with all indnums

  returns:
  - all_data  sender-target dataframe for all senders (indnum=1,2,3,4). Type: panel, three observations per subject 
  "
  data_list <- list()
  for(i in 1:4){
    sender <-  paste0("indnum_",i)
    data_list[[sender]] <- get_sanctions_indnum(data,i)
  }
  all_data <- bind_rows(data_list)
  return(all_data)
}

# do it -------------------------------------------------------------------
all_data <- get_all_sanctions_data(df)
# lag sanctioncost
all_data <- all_data %>% group_by(uniquesubject) %>% 
  mutate(lagsanctioncost = dplyr::lag(sanctioncost, n = 3, default = NA))
# write and save
readstata13::save.dta13(all_data, file = "targeting_data.dta", data.label = "Targeting data (sender to target)", convert.factors = T)

