
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


label define specialization 1 "Traditional" 2 "Egalitarian"


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
*replace  c_`var' = 2 if (av_`var'_share >= 0.45 & av_`var'_share <= 0.55) &  t_event<0 
replace  c_`var' = 2 if av_`var'_share >= 0.5 & t_event<0 


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
keep if sex == 1

label var howlng "Unpaid"
label var jbhrs "Paid"

event_study_two jbhrs howlng "Hours"  t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
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

label var howlng "Unpaid"
label var jbhrs "Paid"

event_study_two jbhrs howlng "Hours" t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_2_male.png", replace


** b - Event study  on female share of total paid/unpaid hours
*-----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 20
keep if sex == 1

label var f_jbhrs_share "Paid hours"
label var f_howlng_share "Unpaid hours"


event_study_two f_jbhrs_share f_howlng_share "Female share"  t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#10"
graph export "${graphs}/spec_3_female.png", replace


*-----------------------> conclusion: main changes occur in the intensive margin of paid hours: 
*                                     women's supply of paid hours is reduced by 8, which corresponds to a drop of 5 p.p. of the total share of paid hours produced by the household
*                                     women's supply of unpaid hours increases by 6, which corresponds to a rise of 10 p.p. of the total share of unpaid hours produced by the household



** c - by type of couple (pre birth specialization): predictive margins 
*-----------------------------------------

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	


label var f_jbhrs_share "female share"


event_margins f_jbhrs_share t_event 1 c_jbhrs "${indiv_c} ${couple_c} ib2.wno" cidp "#10"


*-----------------------> conclusion: women in counter-traditiona couples experience the largest drops in the share of paid hours provided by the hh: loose 10 p.p. of the total share with respect their pre-child position 
*                                    


** d - by type of couple (pre birth specialization) and gender differences 


** Paid hours

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8


* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 2{
	reg jbhrs ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_jbhrs==`qt', vce(cluster cidp)
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
	   (connected M21  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M22 M23 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_3_paid.png", replace


** Unpaid hours

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8


* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 2{
	reg howlng ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_jbhrs==`qt', vce(cluster cidp)
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
	   (connected M21  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M22 M23 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_3_unpaid.png", replace

*-----------------------> conclusion: main changes occur in the intensive margin of paid hours





*============================================================= 
*  3. 2ND STAGE: HETEROGENEITY IN RQ PATHS 
*============================================================= 


** a - by type of couple (pre birth specialization): predictive margins 
*-----------------------------------------

use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

label var f_jbhrs_share "female share"


event_margins rq t_event 1 c_jbhrs "${indiv_c} ${couple_c} ib2.wno" cidp "#10"


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
foreach qt in 1 2{
	reg rq ib`base'.t_event##i.sex ${indiv_c} ${couple_c} ib2.wno if c_jbhrs==`qt', vce(cluster cidp)
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
	   (connected M21  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M22 M23 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(#10, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Counter-traditional") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/spec_3_paid.png", replace




*============================================================= 
*  4. FURTHER DESCRIPTIVE EVIDENCE
*============================================================= 
