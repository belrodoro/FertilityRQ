
/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-01
******************************************************************

This file studies household specialization: distinguish between paid (market work) and unpaid work (domestic work)


Structure of the file:

1 - Classificaiton of couples according to female share of paid and unpaid hours pre child birth
2 - 1st stage: impact of fertility on paid and unpaid hours
               Heterogeneity: gender differences x type of couple
3 - 2nd stage: impact of fertility on RQ by type of couples + 
               Heterogeneity: gender differences x type of couple	
	
	
-----------> main conclusions: there exist substantial gender differentials in the change of hours/female share of total hours of unpaid and paid work. The change is more pronounce for counter-traditional and egalitarian couples.  However this is not translated into lower/different evolution of RQ with fertility. 	
	
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
*  1. Classification of couples according to pre birth outcomes
*============================================================= 

use newparent_couples.dta, clear

** Selection and imputation
drop if t_event < -4 

replace f_jbhrs = . if f_jbhrs < 0
replace m_jbhrs = . if m_jbhrs < 0

replace f_paynu_dv = 0 if f_paynu_dv == . &  (f_jbstat >= 3 & f_jbstat <= 6)
replace m_paynu_dv = 0 if m_paynu_dv == . &  (m_jbstat >= 3 & m_jbstat <= 6)

** Computing gender gaps


label define specialization 1 "Traditional" 2 "Egalitarian" 3 "Counter-traditional"


local gaps "paynu_dv jbhrs howlng"

foreach var of local gaps{
	
if "f_`var'" == "f_howlng"{                                  // so traditional couples are those with large female-to-male relative differences

gen f_`var'_share =  round(1- f_`var'/ (f_`var' + m_`var'), 0.01)

	
}
else {
	
gen f_`var'_share =  round(f_`var'/ (f_`var' + m_`var'), 0.01)
}


egen av_`var'_share = mean(f_`var'_share) if t_event<0, by(cidp)

*xtile  c_`var' = av_`var'_share if t_event<0 , nq(3)

gen      c_`var' = 1 if av_`var'_share < 0.5 & t_event<0                               // ad-hoc classification: with percentiles was a bit unconsistent with "real" understanding of definitions
replace  c_`var' = 2 if (av_`var'_share >= 0.5 & av_`var'_share < 0.6) &  t_event<0 
replace  c_`var' = 3 if av_`var'_share >= 0.6 & t_event<0 


replace  c_`var' = 0 if  c_`var' == . 
ereplace  c_`var' = max( c_`var'), by(cidp)

replace  c_`var' = . if  c_`var' == 0 

label values c_`var' specialization

}

replace f_howlng_share = 1 - f_howlng_share

tempfile categories
keep cidp wave panel f_paynu_dv_share - c_howlng
save `categories', replace

use newparent_sample.dta, clear

merge m:1 cidp wave panel using `categories', keep(1 3) nogen

order pidp cidp year birthy sex age t_event jbstat fimnnet_dv paynu_dv jbhrs howlng f_jbhrs_share c_jbhrs f_howlng_share av_howlng_share c_howlng

*********
save newparent_sample.dta, replace
*********


*============================================================= 
*  2. 1ST STAGE: CHANGES IN OUTCOMES WITH FERTILITY
*============================================================= 

** a - Event study separatedly by gender
*-----------------------------------------

** Earnings (paynu_dv):                                                              // Women 

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
keep if sex == 1

label var paynu_dv "Monthly earnings"

event_study paynu_dv t_event 1  "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_1_female.png", replace

** Intensive margins: paid and unpaid 

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing: we include the extensive margin here

keep if sex == 1

label var howlng "Unpaid"
label var jbhrs "Paid"

event_study_two jbhrs howlng "Hours"  t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_2_female.png", replace

** Earnings (paynu_dv):                                                              // Men  

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
keep if sex == 0

label var paynu_dv "Monthly earnings"

event_study paynu_dv t_event 1  "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_1_male.png", replace


** Intensive margins: paid and unpaid 

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
keep if sex == 0
replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing


label var howlng "Unpaid"
label var jbhrs "Paid"

event_study_two jbhrs howlng "Hours" t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-10(5)10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_2_male.png", replace


** b - Event study  on female share of total paid/unpaid hours
*--------------------------------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
keep if sex == 1

label var f_jbhrs_share "Paid hours"
label var f_howlng_share "Unpaid hours"


event_study_two f_jbhrs_share f_howlng_share "Female share"  t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph display, ysize(5) xsize(9)
graph export "${graphs}/spec_3_female.png", replace


*-----------------------> conclusion: main changes occur in the intensive margin of paid hours: 
*                                     women's supply of paid hours is reduced by 8, which corresponds to a drop of 5 p.p. of the total share of paid hours produced by the household
*                                     women's supply of unpaid hours increases by 6, which corresponds to a rise of 10 p.p. of the total share of unpaid hours produced by the household





** d - by type of couple (pre birth specialization) and gender differences 
*--------------------------------------------------------------------------

** Paid hours

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8
replace jbhrs = 0 if jbhrs ==. & (jbstat >= 3 & jbstat <= 6)          // impute zero paid hours to those who are under maternity leave or unemployed if missing


* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 3{
	reg jbhrs ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_howlng==`qt', vce(cluster cidp)
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
twoway (connected M11  aux   , sort lcolor(`Peach') msymbol("sh") lpattern(dash)  mcolor(`Peach')) ///
	   (rcap      M12 M13 aux ,  sort lcolor(`Peach')) /// 
	   (connected M31  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M32 M33 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_3_paid_ch.png", replace


** Unpaid hours

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8


* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 3{
	reg howlng ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_howlng==`qt', vce(cluster cidp)
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
twoway (connected M11  aux   , sort lcolor(`Peach') msymbol("sh") lpattern(dash)  mcolor(`Peach')) ///
	   (rcap      M12 M13 aux ,  sort lcolor(`Peach')) /// 
	   (connected M31  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M32 M33 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_3_unpaid_ch.png", replace

*-----------------------> conclusion: main changes occur in the intensive margin of paid hours





*============================================================= 
*  3. 2ND STAGE: HETEROGENEITY IN RQ PATHS 
*============================================================= 


** a - by type of couple (pre birth specialization): predictive margins 
*-----------------------------------------

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

label var f_jbhrs_share "female share"

replace c_howlng = . if c_howlng == 2
event_margins rq t_event 1 c_howlng "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_1_rq_ch.png", replace


** d - by type of couple (pre birth specialization) and gender differences 
*-----------------------------------------


use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8


* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 3{
	reg rq ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_howlng==`qt', vce(cluster cidp)
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
twoway (connected M11  aux   , sort lcolor(`Peach') msymbol("sh") lpattern(dash)  mcolor(`Peach')) ///
	   (rcap      M12 M13 aux ,  sort lcolor(`Peach')) /// 
	   (connected M31  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M32 M33 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_4_rq_ch.png", replace


/*

*============================================================= 
*  4. FURTHER DESCRIPTIVE EVIDENCE
*============================================================= 

* a. Cumulative distribution by gender: paid hours
*-------------------------------------------------------------
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
	
	

* b. Within-individual change in working hours: home and market
*-----------------------------------------------------------------
	
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
	   

* c. Within-couple changes in working hours: home and market
*-------------------------------------------------------------
	   
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
	