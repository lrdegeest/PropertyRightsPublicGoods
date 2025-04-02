egen group_id = concat(treatment session group)
egen group_endow_id = concat(treatment session group endowment)

bysort group_id: gen unique_group = runiform() if period > 4
bysort group_id: egen min_value_group = min(unique_group)
bysort group_id: gen is_unique_group = (unique_group == min_value_group)

bysort group_endow_id: gen unique = runiform() if period > 4
bysort group_endow_id: egen min_value = min(unique)
bysort group_endow_id: gen is_unique = (unique == min_value)


bysort group_id: egen average_group_cont_pun = mean(contribute) if period > 4
bysort group_endow_id: egen average_group_cont_low_pun = mean(contribute) if endowment == 30 & period > 4
bysort group_endow_id: egen average_group_cont_high_pun = mean(contribute) if endowment == 70 & period > 4


ttest   average_group_cont_high_pun if is_unique == 1, by(treatment)
power twomeans `r(mu_1)' `r(mu_2)', n1(`r(N_1)') n2(`r(N_2)') sd1(`r(sd_1)') sd2(`r(sd_2)') alpha(0.05)
ranksum average_group_cont_high_pun if is_unique == 1, by(treatment)

ttest   average_group_cont_low_pun if is_unique == 1, by(treatment)
power twomeans `r(mu_1)' `r(mu_2)', n1(`r(N_1)') n2(`r(N_2)') sd1(`r(sd_1)') sd2(`r(sd_2)') alpha(0.05)
ranksum average_group_cont_low_pun if is_unique == 1, by(treatment`)
