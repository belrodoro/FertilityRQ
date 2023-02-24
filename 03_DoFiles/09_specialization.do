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



*============================================================= 
*		          1. Labor market: paid hours
*============================================================= 

* 1 - Cumulative distribution of paid hours

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
	
*----------------------------------------
* 2. Change in working hours: home and market
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
	   xtitle("Change in within-couple gap", size(medsmall)) xlabel(#20, labsize(small)) xline(0, lpattern(dash) lcolor(gs9))  name(men, replace) 		
	graph export "${graphs}/hours_3.png", replace

	
	
	
*----------------------------------------
*4. Within couple changes: hetegoreneity 
*----------------------------------------	
	
sum jbhrs_change, d
gen jbhrs_spe = (jbhrs_change >= `r(p75)')	
	
	
sum hwhrs_change, d
gen hwhrs_spe = (hwhrs_change <= `r(p25)')	
	
	
tab hwhrs_spe jbhrs_spe	
	
sum t_event
local min = -`r(min)' + 1
replace t_event = t_event + `min'

replace jbhrs_spe = 2 if jbhrs_spe == 0

foreach qt in 1 2 {
	reg rq (ib5.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if jbhrs_spe==`qt', vce(cluster cidp)
	* store results in a matrix
	matrix M`qt' = (_b[1.t_event#1.sex], ///
					_b[1.t_event#1.sex] - invttail(e(df_r),0.025)*_se[1.t_event#1.sex] , ///
					_b[1.t_event#1.sex] + invttail(e(df_r),0.025)*_se[1.t_event#1.sex] )	
	qui sum t_event	
	forval xx=2/`r(max)' {
		matrix M`qt' = (M`qt' \ _b[`xx'.t_event#1.sex], ///
					_b[`xx'.t_event#1.sex] - invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] , ///
					_b[`xx'.t_event#1.sex] + invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] )
	}	
	* convert matrix into variables 
	svmat M`qt'	
}
egen aux = seq() if M11!=.
replace aux = aux - `min'

* plot
sum aux
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (rarea  M12 M13 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea  M22 M23 aux ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M11 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol(smsquare_hollow)) ///
	   (connected M21 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol(smsquare_hollow)), ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(-1.5(.5)1, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(3 "Traditional" 4 "Egalitarian") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)




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


