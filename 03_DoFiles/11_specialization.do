/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-01
******************************************************************

This file studies household specialization: distinguish between paid (market work) and unpaid work (domestic work)


Legend: Graph type numbers
		-. pooled 
 		b. significant differences 
 		c. predictive margins 
		d. interacted with sex 
		e. sex differences
		f. sex predictive margins
	
*****************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:/Users/`c(username)'/OneDrive/Documentos/GitHub/FertilityRQ/03_DoFiles/00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"


++++++++++++++++++ THINGS TO DO: 
- compute specialization classification looking at shares: sum up paid and unpaid hours at a couple level, and compute each member relative shares. 


*============================================================= 
*		          1. Labor market: paid hours
*============================================================= 


*----------------------------------------
* 1. Cumulative distribution by gender
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 

label variable jbhrs "Hours Worked"

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing
replace jbhrs = . if jbhrs < 0

* 1. recode factor variable
qui sum t_event 
local min = -`r(min)' + 1
replace t_event = t_event + `min'

// Women
gen jbhrs_pre_w = jbhrs if t_event<=4 & sex == 1
cumul jbhrs_pre_w, generate(cdf_jbhrs_pre_w)

gen jbhrs_post_w = jbhrs if t_event>4 & sex == 1
cumul jbhrs_post_w , generate(cdf_jbhrs_post_w)
	 
	 
	 
colorpalette lin fruits, locals
twoway (lpoly cdf_jbhrs_post_w jbhrs_post_w if sex == 1, lc(`Cherry')) (lpoly cdf_jbhrs_pre_w jbhrs_pre_w if sex == 1, lc(gs11) lp(dash)), ///
	   legend(order(1 "Post child birth" 2 "Pre child birth") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   	title({fontface Arial Narrow:  PANEL A: Women }, size(medsmall)) ///
	   xtitle("Working hours", size(medsmall)) xlabel(,labsize(small)) name(women, replace)
	
// Men
gen jbhrs_pre_m = jbhrs if t_event<=4 & sex == 0
cumul jbhrs_pre_m, generate(cdf_jbhrs_pre_m)

gen jbhrs_post_m = jbhrs if t_event>4 & sex == 0
cumul jbhrs_post_m , generate(cdf_jbhrs_post_m)
	 
	 
	 
colorpalette lin fruits, locals
twoway (lpoly cdf_jbhrs_post_m jbhrs_post_m if sex == 0, lc(`Cherry')) (lpoly cdf_jbhrs_pre_m jbhrs_pre_m if sex == 0, lc(gs11) lp(dash)), ///
	   legend(order(1 "Post child birth" 2 "Pre child birth") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   	title({fontface Arial Narrow:  PANEL B: Men }, size(medsmall)) ///
	   xtitle("Working hours", size(medsmall)) xlabel(,labsize(small)) name(men,replace)
	
	
grc1leg women men,  ycommon legend(women)
graph export "${graphs}/hours_1.png", replace	
	
	
*--------------------------------------------------------
* 2. Event study 
*---------------------------------------------------------	

** By unpaid division of labor
*---------------------------------

** Gender differences if traditional couples 
use newparent_sample.dta, clear

drop if t_event < -7 | t_event > 7	
label variable jbhrs "Hours Worked"

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing
replace jbhrs = . if jbhrs < 0

keep if gbeh_q == 1
event_difference jbhrs t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/paid_trad_diff.png", replace

** Gender differences if egalitarian couples 
use newparent_sample.dta, clear

drop if t_event < -7 | t_event > 7	
label variable jbhrs "Hours Worked"

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing
replace jbhrs = . if jbhrs < 0


keep if gbeh_q == 2
event_difference jbhrs t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/paid_eg_diff.png", replace


	
	
*------------> conclusion: egalitarian couples have the largest gender differentials in the impact of fertility on paid working hours
	
	
	
	
*============================================================= 
*    2. Differences in paid and unpaid margins: DESCRIPTIVE
*============================================================= 	
	
*----------------------------------------
* a. Change in working hours: home and market
*----------------------------------------
	
** Paid hours: 

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7                  // restrict penalty to a seven years window
keep if panel == "UKHLS"

label variable jbhrs "Hours Worked"

* Impute zero hours if nonemployed

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)
replace jbhrs = . if jbhrs < 0

	
* Pre and Post-bith Average paid hours 
egen jbhrs_pre  = mean(jbhrs) if t_event <0, by(pidp)
egen jbhrs_post = mean(jbhrs) if t_event >=0, by(pidp)

ereplace jbhrs_pre = max(jbhrs_pre), by(pidp)
ereplace jbhrs_post = max(jbhrs_post), by(pidp)

gen jbhrs_diff = jbhrs_post - jbhrs_pre 	


** Unpaid hours 

replace howlng = . if howlng < 0

egen hwhrs_pre  = mean(howlng) if t_event <0, by(pidp)
egen hwhrs_post = mean(howlng) if t_event >=0, by(pidp)

ereplace hwhrs_pre = max(hwhrs_pre), by(pidp)
ereplace hwhrs_post = max(hwhrs_post), by(pidp)

gen hwhrs_diff =  hwhrs_post - hwhrs_pre

	
	
// CDF Women
gen jbhrs_diff_w = jbhrs_diff if sex == 1
cumul jbhrs_diff_w, generate(cdf_jbhrs_diff)

gen hwhrs_diff_w = hwhrs_diff if sex == 1
cumul hwhrs_diff_w, generate(cdf_hwhrs_diff)

colorpalette lin fruits, locals
twoway (lpoly cdf_jbhrs_diff jbhrs_diff_w if sex == 1, lc(`Cherry'))  (lpoly cdf_hwhrs_diff hwhrs_diff_w if sex == 1, lc(gs11) lp(dash)), ///
	   legend(order(1 "Paid hours" 2 "Unpaid hours") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   xtitle("Working hours", size(medsmall)) xlabel(,labsize(small)) 

	   
// PDF Women	   
colorpalette lin fruits algorithm, local opacity(60)

twoway (histogram  jbhrs_diff_w if sex == 1,  color(`Cherry') )  ///
       (histogram  hwhrs_diff_w if sex == 1,  color(`Blueberry') ),  ///
	   legend(order(1 "Paid hours" 2 "Unpaid hours") row(1) pos(north) size(small)) ///
	   ytitle("Density", size(medsmall)) ylabel(#10, labsize(small)) ///
	   	title({fontface Arial Narrow:  PANEL A: Women }, size(medsmall)) ///
	   xtitle("Change in working hours", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9)) name(women, replace)  
	   
	   
	   
// CDF Men
gen jbhrs_diff_m = jbhrs_diff if sex == 0
cumul jbhrs_diff_m, generate(cdf_jbhrs_diff_m)

gen hwhrs_diff_m = hwhrs_diff if sex == 0
cumul hwhrs_diff_m, generate(cdf_hwhrs_diff_m)

colorpalette lin fruits, locals
twoway (lpoly cdf_jbhrs_diff_m jbhrs_diff_m if sex == 0, lc(`Cherry'))  (lpoly cdf_hwhrs_diff_m hwhrs_diff_m if sex == 0, lc(gs11) lp(dash)), ///
	   legend(order(1 "Paid hours" 2 "Unpaid hours") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   xtitle("Working hours", size(medsmall)) xlabel(,labsize(small)) 	   
	   
	   
// PDF Men	   
colorpalette lin fruits algorithm, local opacity(60)

twoway (histogram  jbhrs_diff_m if sex == 0,  color(`Cherry') )  ///
       (histogram  hwhrs_diff_m if sex == 0,  color(`Blueberry') ),  ///
	   legend(order(1 "Paid hours" 2 "Unpaid hours") row(1) pos(north) size(small)) ///
	   ytitle("Density", size(medsmall)) ylabel(#10, labsize(small)) ///
	   title({fontface Arial Narrow:  PANEL B: Men }, size(medsmall)) ///
	   xtitle("Change in working hours", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9))  name(men, replace) 	   
	   
	   
	   
grc1leg women men,  ycommon legend(women)
graph export "${graphs}/hours_2.png", replace
	   
*----------------------------------------
*3. Within couple changes: distribution 
*----------------------------------------
	   
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7
keep if panel == "UKHLS"


*** Paid hours 

* Impute zero hours if nonemployed

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)
egen jbhrs_pre  = mean(jbhrs) if t_event <0, by(pidp)
egen jbhrs_post = mean(jbhrs) if t_event >=0, by(pidp)

ereplace jbhrs_pre = max(jbhrs_pre), by(pidp)
ereplace jbhrs_post = max(jbhrs_post), by(pidp)
	   
* Within couple differences: 	   

// Pre child birth 
gen jbhrs_pre_w = jbhrs_pre if sex == 1 
gen jbhrs_pre_m = jbhrs_pre if sex == 0
  
ereplace jbhrs_pre_w = max(jbhrs_pre_w), by(cidp) 
ereplace jbhrs_pre_m = max(jbhrs_pre_m), by(cidp)  

gen jbhrs_diff_pre = jbhrs_pre_w - jbhrs_pre_m 

// Post child birth 
gen jbhrs_post_w = jbhrs_post if sex == 1 
gen jbhrs_post_m = jbhrs_post if sex == 0
  
ereplace jbhrs_post_w = max(jbhrs_post_w), by(cidp) 
ereplace jbhrs_post_m = max(jbhrs_post_m), by(cidp)  
 
gen jbhrs_diff_post = jbhrs_post_w - jbhrs_post_m 

gen jbhrs_change = jbhrs_diff_post - jbhrs_diff_pre 
	   
*** Unpaid hours 

* Impute zero hours if nonemployed
replace howlng = . if howlng < 0

egen hwhrs_pre  = mean(howlng) if t_event <0, by(pidp)
egen hwhrs_post = mean(howlng) if t_event >=0, by(pidp)

ereplace hwhrs_pre = max(hwhrs_pre), by(pidp)
ereplace hwhrs_post = max(hwhrs_post), by(pidp)
	   
* Within couple differences: 	   

// Pre child birth 
gen hwhrs_pre_w = hwhrs_pre if sex == 1 
gen hwhrs_pre_m = hwhrs_pre if sex == 0
  
ereplace hwhrs_pre_w = max(hwhrs_pre_w), by(cidp) 
ereplace hwhrs_pre_m = max(hwhrs_pre_m), by(cidp)  

gen hwhrs_diff_pre =  hwhrs_pre_m - hwhrs_pre_w 

// Post child birth 
gen hwhrs_post_w = hwhrs_post if sex == 1 
gen hwhrs_post_m = hwhrs_post if sex == 0
  
ereplace hwhrs_post_w = max(hwhrs_post_w), by(cidp) 
ereplace hwhrs_post_m = max(hwhrs_post_m), by(cidp)  
 
gen hwhrs_diff_post = hwhrs_post_w - hwhrs_post_m 

gen hwhrs_change =  hwhrs_diff_post -hwhrs_diff_pre 

	   
colorpalette lin fruits algorithm, local opacity(60)

twoway (histogram  jbhrs_diff_pre  ,  color(`Cherry') )  ///
       (histogram  jbhrs_diff_post,  color(`Blueberry') ),  ///
	   legend(order(1 "Pre child birth" 2 "Post child birth") row(1) pos(north) size(small)) ///
	   ytitle("Density", size(medsmall)) ylabel(#10, labsize(small)) ///
	   title({fontface Arial Narrow:  PANEL A: Paid hours }, size(medsmall)) ///
	   xtitle("Women relative to men", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9))  name(men, replace) 
	   
	   
colorpalette lin fruits algorithm, local opacity(60)

twoway (histogram  hwhrs_diff_pre  ,  color(`Cherry'))  ///
       (histogram  hwhrs_diff_post,  color(`Blueberry') ),  ///
	   legend(order(1 "Pre child birth" 2 "Post child birth") row(1) pos(north) size(small)) ///
	   ytitle("Density", size(medsmall)) ylabel(#10, labsize(small)) ///
	   title({fontface Arial Narrow:  PANEL B: Unpaid hours }, size(medsmall)) ///
	   xtitle("Women relative to men", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9))  name(men, replace) 	
	
// Relative change within the couple : graph showing specialization within couple 
	
colorpalette lin fruits algorithm, local opacity(60)

twoway (histogram  jbhrs_change  ,  color(`Cherry') )  ///
       (histogram  hwhrs_change,  color(`Blueberry') ),  ///
	   legend(order(1 "Paid work" 2 "Unpaid work") row(1) pos(north) size(small)) ///
	   ytitle("Density", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Change in the within-couple gap", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9))  name(men, replace) 		
	graph export "${graphs}/hours_3.png", replace

	
	
*============================================================= 
*    3. Differences in paid and unpaid margins: INDEX
*============================================================= 	
	
	
** Paid hours: 

use newparent_sample.dta, clear
drop if t_event < -4 
keep if panel == "UKHLS"

label variable jbhrs "Hours Worked"

* Impute zero hours if nonemployed

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)
replace jbhrs = . if jbhrs < 0


** Absolute differences:

gen jbhrs_w = jbhrs if sex == 1
gen jbhrs_m = jbhrs if sex == 0

gen hwhrs_w = howlng if sex == 1
gen hwhrs_m = howlng if sex == 0


foreach var in jbhrs_w jbhrs_m hwhrs_w hwhrs_m{
	
ereplace `var' = max(`var'), by(cidp t_event)
	
}

gen jbhrs_diff = abs(jbhrs_w - jbhrs_m)
gen hwhrs_diff = abs(hwhrs_w - hwhrs_m)

gen total_diff = jbhrs_diff + hwhrs_diff   // this measure is agregated at the couple level

egen aux1 = seq(),by(cidp t_event)
drop if aux1> 1

label var jbhrs_diff "paid hours"       // differences in paid hours 
label var hwhrs_diff "unpaid hours"       // differences in unpaid hours 

		
** Together
event_study_two jbhrs_diff hwhrs_diff "hours" t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)25"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hours_4.png", replace
	
	
** Total hours of specialization 
label var total_diff "specialization (hours)"       // total specialization 
event_study total_diff t_event 1 "" cidp "-10(5)25"
graph display, ysize(5) xsize(9)


*event_study jbhrs_diff t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)25"
*graph display, ysize(5) xsize(9)
*	
*event_study hwhrs_diff t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)25"
*graph display, ysize(5) xsize(9)
		
	
	
*----------------------------------------
*4. Within couple changes: hetegoreneity 
*----------------------------------------	

use newparent_sample.dta, clear
drop if t_event < -4 
keep if panel == "UKHLS"

label variable jbhrs "Hours Worked"

* Impute zero paid hours if nonemployed

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)
replace jbhrs = . if jbhrs < 0


** Absolute differences:

gen jbhrs_w = jbhrs if sex == 1
gen jbhrs_m = jbhrs if sex == 0

gen hwhrs_w = howlng if sex == 1
gen hwhrs_m = howlng if sex == 0

// Create variable at couple level 
foreach var in jbhrs_w jbhrs_m hwhrs_w hwhrs_m{
	
ereplace `var' = max(`var'), by(cidp t_event)
	
}

gen jbhrs_diff = abs(jbhrs_w - jbhrs_m)
gen hwhrs_diff = abs(hwhrs_w - hwhrs_m)

gen total_diff = jbhrs_diff + hwhrs_diff   // this measure is agregated at the couple level
	
** Household classification into specialized/non specialized according to pre-child levels of specialization	
	
egen total_diff_pre = mean(total_diff) if t_event <= -1	, by(cidp) 
egen jbhrs_diff_pre = mean(jbhrs_diff) if t_event <= -1	, by(cidp) 
egen hwhrs_diff_pre = mean(total_diff) if t_event <= -1	, by(cidp) 

// Create variable at a couple level 
foreach var in total_diff_pre jbhrs_diff_pre hwhrs_diff_pre{
	
ereplace `var' = max(`var') if `var'!=., by(cidp)
	
}
	
sum total_diff_pre if total_diff_pre != ., d
gen total_spe = (total_diff_pre >= `r(p75)')   & total_diff_pre !=.            // couples in the botton quartile had weekly differences of sixteen hours.
replace total_spe = . if total_diff_pre == . 
	
sum jbhrs_diff_pre if jbhrs_diff_pre!= . , d
gen jbhrs_spe = (jbhrs_diff_pre >= `r(p75)')  & jbhrs_diff_pre !=. 	           // couples in the botton quartile had paid weekly differences of ten hours.	
replace jbhrs_spe = . if jbhrs_diff_pre == . 
	
sum hwhrs_diff_pre if hwhrs_diff_pre!= . ,  d
gen hwhrs_spe = (hwhrs_diff_pre >= `r(p75)')	 & hwhrs_diff_pre !=.          // couples in the botton quartile had paid weekly differences of seven hours.	
replace hwhrs_spe = . if hwhrs_diff_pre == . 		
		
// Extrapolate pre-birth classification to post-birth leads
		
foreach var in hwhrs_spe jbhrs_spe total_spe{

replace `var' = - 1 if `var' == .	
ereplace `var' = max(`var') , by(cidp)

replace `var' =. if `var' == -1	
}		
		
		
sort cidp pidp year 
order pidp year sex age cidp t_event jbstat jbhrs jbhrs_w jbhrs_m jbhrs_diff howlng hwhrs_w hwhrs_m hwhrs_diff total_diff_pre total_spe jbhrs_spe hwhrs_spe	
		
		
tab hwhrs_spe jbhrs_spe	

replace total_spe = 2 if total_spe == 0 

label def total_spe 1 "Specialized" 2 "Non-specialized"
label val total_spe total_spe 
label var total_diff "specialization (hours)"


*** Heterogeneity in specialization: both margins together

preserve	
event_margins total_diff t_event 1 total_spe "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)25"
graph display, ysize(5) xsize(5)
restore

** Paid margin 
preserve	
event_margins jbhrs_diff t_event 1 total_spe "${indiv_c} ${couple_c} ib2.wno" cidp "-20(5)25"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hours_5.png", replace

restore 

** Unpaid margin
preserve	
event_margins hwhrs_diff t_event 1 total_spe "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)25"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hours_6.png", replace
restore  

* conclusion ---------------> largest changes in specialization happen in the paid margin 




*** Heterogeneity by RQ 


replace jbhrs_spe = 2 if jbhrs_spe == 0 

label def jbhrs_spe 1 "Specialized" 2 "Non-specialized"
label val jbhrs_spe jbhrs_spe 
label var total_diff "specialization (Paid hours)"

preserve	
event_margins rq t_event 1 jbhrs_spe "${indiv_c} ${couple_c} ib2.wno" cidp "-2.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hours_7.png", replace

restore 


*----------------------------------------
*4. Women changes: hetegoreneity 
*----------------------------------------	
	
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7
keep if panel == "UKHLS"


*** Average paid hours 

* Impute zero hours if nonemployed

replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)
egen jbhrs_pre  = mean(jbhrs) if t_event <0, by(pidp)
egen jbhrs_post = mean(jbhrs) if t_event >=0, by(pidp)

ereplace jbhrs_pre = max(jbhrs_pre), by(pidp)
ereplace jbhrs_post = max(jbhrs_post), by(pidp)
	   
* Women changes: 	   

gen jbhrs_change_w = jbhrs_post - jbhrs_pre if sex == 1
  
sum jbhrs_change_w, d	
gen largest_change_w = (jbhrs_change_w >= `r(p75)')
	
label def largest_change_w 0 "Traditional arragement" 1 "Equal arragement"	
label val largest_change_w largest_change_w
	
event_margins rq t_event 1 largest_change_w "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)	
	
	
sum t_event
local min = -`r(min)' + 1
replace t_event = t_event + `min'

reg rq ib5.t_event##ib0.largest_change_w ${indiv_c} ${couple_c} ib2.wno if sex == 1, vce(cluster cidp)


