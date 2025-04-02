
/*
Property Rights and Public Goods - DO File
Treatment: 2 = Unequal Unearned, 3 = Unequal Earned
Period 1 = Slider Task
Period 2 - 4 = VCM
Period 5-34 = peer pun
*/

****** group IDs
egen group_id = concat(treatment session group)
table group_id 
egen group_endow_id = concat(treatment session group endowment)
table group_endow_id 

**** create unique ID to count observations
bysort group_id: gen unique_group = runiform() if period > 4
bysort group_id: egen min_value_group = min(unique_group)
bysort group_id: gen is_unique_group = (unique_group == min_value_group)

bysort group_endow_id: gen unique = runiform() if period > 4
bysort group_endow_id: egen min_value = min(unique)
bysort group_endow_id: gen is_unique = (unique == min_value)

***** subject ID
egen subject_id = concat(treatment session subject)
xtset subject_id

*======================================
*     Contributions 
*======================================
bysort group_id: egen average_group_cont_pun = mean(contribute) if period > 4
bysort group_endow_id: egen average_group_cont_low_pun = mean(contribute) if endowment == 30 & period > 4
bysort group_endow_id: egen average_group_cont_high_pun = mean(contribute) if endowment == 70 & period > 4

**** Table 2
table treatment endowment if period > 4, stat(mean contribute) stat(sd contribute)
**** Signrank
signrank   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 2
signrank   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 3
signrank   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 2
signrank   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 3
**** Ranksum
ranksum average_group_cont_pun if is_unique_group, by(treatment)
ranksum average_group_cont_high_pun if is_unique == 1, by(treatment)
ranksum average_group_cont_low_pun if is_unique == 1, by(treatment)

*======================================
*       Sanctions Sent/Received 
*======================================
bysort group_id: egen average_group_pun_sent = mean(admincost) if period > 4
bysort group_id: egen average_group_pun_rec = mean(sanctioncost) if period > 4
bysort group_endow_id: egen ave_group_pun_sent_low = mean(admincost) if endowment == 30 & period > 4
bysort group_endow_id: egen ave_group_pun_sent_high = mean(admincost) if endowment == 70 & period > 4
bysort group_endow_id: egen ave_group_pun_rec_low = mean(sanctioncost) if endowment == 30 & period > 4
bysort group_endow_id: egen ave_group_pun_rec_high = mean(sanctioncost) if endowment == 70 & period > 4

**** Table 3
table treatment endowment if period > 4, stat(mean admincost) stat(sd admincost)
table treatment endowment if period > 4, stat(mean sanctioncost) stat(sd sanctioncost)
**** Ranksum
ranksum average_group_pun_sent if is_unique_group == 1, by(treatment)
ranksum average_group_pun_rec if is_unique_group == 1, by(treatment)
ranksum ave_group_pun_sent_low if is_unique == 1, by(treatment)
ranksum ave_group_pun_rec_low if is_unique == 1, by(treatment)
ranksum ave_group_pun_sent_high if is_unique == 1, by(treatment)
ranksum ave_group_pun_rec_high if is_unique == 1, by(treatment)

**** Table 4
**** Deviation from Proportional Norm
gen pos_dev_high = contribute - 35 if endowment == 70 & contribute >= 35
replace pos_dev_high = 0 if pos_dev_high == .
gen neg_dev_high = 35 - contribute if endowment == 70 & contribute <=35
replace neg_dev_high = 0 if neg_dev_high == .

xtreg sanctioncost i.treatment#c.neg_dev_high i.treatment#c.pos_dev_high if endowment == 70 & period > 4, fe vce(cluster group_id)
margins, dydx(treatment) at((mean) neg_dev_high)

gen pos_dev_low = contribute - 15 if endowment == 30 & contribute >= 15
replace pos_dev_low = 0 if pos_dev_low == .
gen neg_dev_low = 15 - contribute if endowment == 30 & contribute <= 15
replace neg_dev_low = 0 if neg_dev_low == .

xtreg sanctioncost i.treatment#c.pos_dev_low i.treatment#c.neg_dev_low if endowment == 30 & period > 4, fe vce(cluster group_id)
margins, dydx(treatment) at((mean) neg_dev_low)

*======================================
*     Earnings 
*======================================
bysort group_id: egen average_group_profit_pun = mean(profit) if period > 4
bysort group_endow_id: egen average_group_profit_low_pun = mean(profit) if endowment == 30 & period > 4
bysort group_endow_id: egen average_group_profit_high_pun = mean(profit) if endowment == 70 & period > 4

**** Table 5
table treatment endowment if period > 4, stat(mean profi) stat(sd profit)
**** Ranksum
ranksum average_group_profit_pun if is_unique_group == 1, by(treatment)
ranksum average_group_profit_high_pun if is_unique == 1, by(treatment)
ranksum average_group_profit_low_pun if is_unique == 1, by(treatment)
**** Signrank
gen ave_profit_low_earn    = average_group_profit_low_pun if treatment == 3
gen ave_profit_low_unearn  = average_group_profit_low_pun if treatment == 2
gen ave_profit_high_earn   = average_group_profit_high_pun if treatment == 3
gen ave_profit_high_unearn = average_group_profit_high_pun if treatment == 2

sort group_id period
replace ave_profit_low_earn 	   	= ave_profit_low_earn[_n-1] if ave_profit_low_earn == . & group_id == group_id[_n-1]
replace ave_profit_low_unearn   	= ave_profit_low_unearn[_n-1] if ave_profit_low_unearn == . & group_id == group_id[_n-1]
replace ave_profit_high_earn    	= ave_profit_high_earn[_n-1] if ave_profit_high_earn == . & group_id == group_id[_n-1]
replace ave_profit_high_unearn  	= ave_profit_high_unearn[_n-1] if ave_profit_high_unearn == . & group_id == group_id[_n-1]

signrank ave_profit_low_earn = ave_profit_high_earn if period == 34 & indnum == 1 & treatment == 3
signrank ave_profit_low_unearn = ave_profit_high_unearn if period == 34 & indnum == 1 & treatment == 2	


*======================================
* Appendix: Contributions and Earnings 
*======================================
**** Table 6
table treatment endowment if period < 4, stat(mean contribute) stat(sd contribute)
table treatment endowment if period < 4, stat(mean profit) stat(sd profit)

**** create unique ID to count observations
bysort group_endow_id: gen unique_vcm = runiform() if period < 4
bysort group_endow_id: egen min_value_vcm = min(unique_vcm)
bysort group_endow_id: gen is_unique_vcm = (unique_vcm == min_value_vcm)

bysort group_id: gen unique_group_vcm = runiform() if period < 4
bysort group_id: egen min_value_group_vcm = min(unique_group_vcm)
bysort group_id: gen is_unique_group_vcm = (unique_group_vcm == min_value_group_vcm)

**** Generate Data
bysort group_id: egen average_group_cont_vcm = 		 mean(contribute) if period < 4
bysort group_id: egen average_group_profit_vcm = 	 mean(profit) if period < 4
bysort group_endow_id: egen average_group_cont_low_vcm =     mean(contribute) if endowment == 30 & period < 4
bysort group_endow_id: egen average_group_cont_high_vcm =    mean(contribute) if endowment == 70 & period < 4
bysort group_endow_id: egen average_group_profit_low_vcm =     mean(profit) if endowment == 30 & period < 4
bysort group_endow_id: egen average_group_profit_high_vcm =    mean(profit) if endowment == 70 & period < 4

**** Contributions
ranksum average_group_cont_vcm   if is_unique_group_vcm == 1 & treatment != 1, by(treatment)
ranksum average_group_cont_low_vcm if is_unique_vcm == 1, by(treatment)
ranksum average_group_cont_high_vcm if is_unique_vcm == 1, by(treatment)

**** Earnings
ranksum average_group_profit_vcm   if is_unique_group_vcm & treatment != 1, by(treatment)
ranksum average_group_profit_high_vcm if is_unique_vcm == 1, by(treatment)
ranksum average_group_profit_low_vcm if is_unique_vcm == 1, by(treatment)
