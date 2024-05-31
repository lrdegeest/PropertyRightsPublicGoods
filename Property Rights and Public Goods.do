
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

** create unique ID for non-parametric tests
bysort group_endow_id: gen unique = runiform() if period > 4
bysort group_endow_id: egen min_value = min(unique)
bysort group_endow_id: gen is_unique = (unique == min_value)
bysort group_id: gen unique_group = runiform() if period > 4
bysort group_id: egen min_value_group = min(unique_group)
bysort group_id: gen is_unique_group = (unique_group == min_value_group)
gen middle = 1 if period > 10 & period < 30
bysort group_endow_id: gen unique_m = runiform() if middle == 1
bysort group_endow_id: egen min_value_m = min(unique_m)
bysort group_endow_id: gen is_unique_m = (unique_m == min_value_m)
bysort group_id: gen unique_group_m = runiform() if middle == 1
bysort group_id: egen min_value_group_m = min(unique_group_m)
bysort group_id: gen is_unique_group_m = (unique_group_m == min_value_group_m)

***** subject ID
egen subject_id = concat(treatment session subject)
xtset subject_id

*======================================
*     Contributions 
*======================================
bysort group_id: egen average_group_cont_pun = mean(contribute) if period > 4
bysort group_id: egen average_group_cont_pun_m = mean(contribute) if middle == 1
bysort group_endow_id: egen average_group_cont_low_pun = mean(contribute) if endowment == 30 & period > 4
bysort group_endow_id: egen average_group_cont_high_pun = mean(contribute) if endowment == 70 & period > 4
bysort group_endow_id: egen average_group_cont_low_pun_m = mean(contribute) if endowment == 30 & middle == 1
bysort group_endow_id: egen average_group_cont_high_pun_m = mean(contribute) if endowment == 70 & middle == 1

**** Table 2, Footnotes 5 & 6
table treatment endowment if period > 4, stat(mean contribute) stat(sd contribute)

ttest   average_group_cont_pun if is_unique_group, by(treatment)
ranksum average_group_cont_pun if is_unique_group, by(treatment)
ttest   average_group_cont_high_pun if is_unique == 1, by(treatment)
ranksum average_group_cont_high_pun if is_unique == 1, by(treatment)
ttest   average_group_cont_low_pun if is_unique == 1, by(treatment)
ranksum average_group_cont_low_pun if is_unique == 1, by(treatment)

ttest   average_group_cont_pun_m if is_unique_group_m == 1 by(treatment)
ranksum average_group_cont_pun_m if is_unique_group_m == 1, by(treatment)
ttest   average_group_cont_high_pun_m if is_unique_m == 1, by(treatment)
ranksum average_group_cont_high_pun_m if is_unique_m == 1, by(treatment)
ttest   average_group_cont_low_pun_m if is_unique_m == 1, by(treatment)
ranksum average_group_cont_low_pun_m if is_unique_m == 1, by(treatment)

**** Footnote 7
ttest   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 2
signrank   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 2
ttest   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 3
signrank   	average_group_cont_high_pun = 35 if is_unique == 1 & endowment == 70 & treatment == 3
ttest   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 2
signrank   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 2
ttest   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 3
signrank   	average_group_cont_low_pun = 15 if is_unique == 1 & endowment == 30 & treatment == 3

ttest   	average_group_cont_high_pun_m = 35 if is_unique_m == 1 & endowment == 70 & treatment == 2
signrank   	average_group_cont_high_pun_m = 35 if is_unique_m == 1 & endowment == 70 & treatment == 2
ttest   	average_group_cont_high_pun_m = 35 if is_unique_m == 1 & endowment == 70 & treatment == 3
signrank   	average_group_cont_high_pun_m = 35 if is_unique_m == 1 & endowment == 70 & treatment == 3
ttest   	average_group_cont_low_pun_m = 15 if is_unique_m == 1 & endowment == 30 & treatment == 2
signrank   	average_group_cont_low_pun_m = 15 if is_unique_m == 1 & endowment == 30 & treatment == 2
ttest   	average_group_cont_low_pun_m = 15 if is_unique_m == 1 & endowment == 30 & treatment == 3
signrank   	average_group_cont_low_pun_m = 15 if is_unique_m == 1 & endowment == 30 & treatment == 3

*======================================
*       Sanctions Sent/Received 
*======================================
bysort group_id: egen average_group_pun_sent = mean(admincost) if period > 4
bysort group_id: egen average_group_pun_rec = mean(sanctioncost) if period > 4
bysort group_id: egen average_group_pun_sent_m = mean(admincost) if middle == 1
bysort group_id: egen average_group_pun_rec_m = mean(sanctioncost) if middle == 1

bysort group_endow_id: egen ave_group_pun_sent_low = mean(admincost) if endowment == 30 & period > 4
bysort group_endow_id: egen ave_group_pun_sent_high = mean(admincost) if endowment == 70 & period > 4
bysort group_endow_id: egen ave_group_pun_sent_low_m = mean(admincost) if endowment == 30 & middle == 1
bysort group_endow_id: egen ave_group_pun_sent_high_m = mean(admincost) if endowment == 70 & middle == 1

bysort group_endow_id: egen ave_group_pun_rec_low = mean(sanctioncost) if endowment == 30 & period > 4
bysort group_endow_id: egen ave_group_pun_rec_high = mean(sanctioncost) if endowment == 70 & period > 4
bysort group_endow_id: egen ave_group_pun_rec_low_m = mean(sanctioncost) if endowment == 30 middle == 1
bysort group_endow_id: egen ave_group_pun_rec_high_m = mean(sanctioncost) if endowment == 70 middle == 1

**** Table 3, Footnote 8
table treatment endowment if period > 4, stat(mean admincost) stat(sd admincost)
table treatment endowment if period > 4, stat(mean sanctioncost) stat(sd sanctioncost)

ttest   average_group_pun_sent if is_unique_group == 1, by(treatment)
ranksum average_group_pun_sent if is_unique_group == 1, by(treatment)
ttest   average_group_pun_rec if is_unique_group == 1, by(treatment)
ranksum average_group_pun_rec if is_unique_group == 1, by(treatment)
ttest   average_group_pun_sent_m if is_unique_group_m == 1, by(treatment)
ranksum average_group_pun_sent_m if is_unique_group_m == 1, by(treatment)
ttest   average_group_pun_rec_m if is_unique_group_m == 1, by(treatment)
ranksum average_group_pun_rec_m if is_unique_group_m == 1, by(treatment)

ttest   ave_group_pun_sent_low if is_unique == 1, by(treatment)
ranksum ave_group_pun_sent_low if is_unique == 1, by(treatment)
ttest   ave_group_pun_rec_low if is_unique == 1, by(treatment)
ranksum ave_group_pun_rec_low if is_unique == 1, by(treatment)
ttest   ave_group_pun_sent_low_m if is_unique_m == 1, by(treatment)
ranksum ave_group_pun_sent_low_m if is_unique_m == 1, by(treatment)
ttest   ave_group_pun_rec_low_m if is_unique_m == 1, by(treatment)
ranksum ave_group_pun_rec_low_m if is_unique_m == 1, by(treatment)

ttest   ave_group_pun_sent_high if is_unique == 1, by(treatment)
ranksum ave_group_pun_sent_high if is_unique == 1, by(treatment)
ttest   ave_group_pun_rec_high if is_unique == 1, by(treatment)
ranksum ave_group_pun_rec_high if is_unique == 1, by(treatment)
ttest   ave_group_pun_sent_high_m if is_unique_m == 1, by(treatment)
ranksum ave_group_pun_sent_high_m if is_unique_m == 1, by(treatment)
ttest   ave_group_pun_rec_high_m if is_unique_m == 1, by(treatment)
ranksum ave_group_pun_rec_high_m if is_unique_m == 1, by(treatment)

**** Table 4
****** Deviation from Proportional Norm
gen pos_dev_high = contribute - 35 if endowment == 70 & contribute >= 35
replace pos_dev_high = 0 if pos_dev_high == .
gen neg_dev_high = 35 - contribute if endowment == 70 & contribute <=35
replace neg_dev_high = 0 if neg_dev_high == .

xtreg sanctioncost i.treatment#c.neg_dev_high i.treatment#c.pos_dev_high if endowment == 70 & period > 4, vce(cluster group_id)
xtreg sanctioncost i.treatment#c.neg_dev_high i.treatment#c.pos_dev_high if endowment == 70 & middle == 1, vce(cluster group_id)
margins, dydx(treatment) at((mean) neg_dev_high)

gen pos_dev_low = contribute - 15 if endowment == 30 & contribute >= 15
replace pos_dev_low = 0 if pos_dev_low == .
gen neg_dev_low = 15 - contribute if endowment == 30 & contribute <= 15
replace neg_dev_low = 0 if neg_dev_low == .

xtreg sanctioncost i.treatment#c.pos_dev_low i.treatment#c.neg_dev_low if endowment == 30 & period > 4, vce(cluster group_id)
xtreg sanctioncost i.treatment#c.pos_dev_low i.treatment#c.neg_dev_low if endowment == 30 & middle == 1, vce(cluster group_id)
margins, dydx(treatment) at((mean) neg_dev_low)

*======================================
*     Earnings 
*======================================
bysort group_id: egen average_group_profit_pun = mean(profit) if period > 4
bysort group_id: egen average_group_profit_m = mean(profit) if middle == 1

bysort group_endow_id: egen average_group_profit_low_pun = mean(profit) if endowment == 30 & period > 4
bysort group_endow_id: egen average_group_profit_high_pun = mean(profit) if endowment == 70 & period > 4
bysort group_endow_id: egen average_group_profit_low_m = mean(profit) if endowment == 30 & middle == 1
bysort group_endow_id: egen average_group_profit_high_m = mean(profit) if endowment == 70 & middle == 1

**** Table 5, Footnote 9
table treatment endowment if period > 4, stat(mean profi) stat(sd profit)

ttest   average_group_profit_pun if is_unique_group == 1, by(treatment)
ranksum average_group_profit_pun if is_unique_group == 1, by(treatment)
ttest   average_group_profit_high_pun if is_unique == 1, by(treatment)
ranksum average_group_profit_high_pun if is_unique == 1, by(treatment)
ttest   average_group_profit_low_pun if is_unique == 1, by(treatment)
ranksum average_group_profit_low_pun if is_unique == 1, by(treatment)

ttest   average_group_profit_m if is_unique_group_m == 1, by(treatment)
ranksum average_group_profit_m if is_unique_group_m == 1, by(treatment)
ttest   average_group_profit_high_m if is_unique_m == 1 & endowment == 70, by(treatment)
ranksum average_group_profit_high_m if is_unique_m == 1 & endowment == 70, by(treatment)
ttest   average_group_profit_low_m if is_unique_m == 1 & endowment == 30, by(treatment)
ranksum average_group_profit_low_m if is_unique_m == 1 & endowment == 30, by(treatment)