/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-01
******************************************************************

This file studies heterogeneity on the main effect

List of variables:
		3. domestic division of labor 
		4. gender role attitudes 
		5. child sex 
		6. education
		7. assortative mating
		8. urban areas

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
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"

*============================================================= 
*		3. domestic labor division
*============================================================= 

*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

event_margins rq t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_03.png", replace

*----------------------------------------
* b. significant differences 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_difference rq t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_03b.png", replace

*----------------------------------------
* e. interacted with sex difference 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 6	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 2 {
	reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if gbeh_q==`qt', vce(cluster cidp)
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
graph export "${graphs}/rq_03e.png", replace

*============================================================= 
*		4. gender role attitudes 
*============================================================= 
*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

event_margins rq t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_04.png", replace

*----------------------------------------
* b. significant differences 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_difference rq t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_04b.png", replace

*----------------------------------------
* e. interacted with sex difference 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 6	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 1 2 {
	reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if gnorm_q==`qt', vce(cluster cidp)
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
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Conservative" 3 "Progressive") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_04e.png", replace


*============================================================= 
*		5. child sex 
*============================================================= 
*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

event_margins rq t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_05.png", replace

*----------------------------------------
* e. interacted with sex difference 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 6	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* separately per group 
foreach qt in 0 1 {
	reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if ch_sex==`qt', vce(cluster cidp)
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
twoway (connected M01  aux   , sort lcolor(`Peach') msymbol("sh") lpattern(dash)  mcolor(`Peach')) ///
	   (rcap      M02 M03 aux ,  sort lcolor(`Peach')) /// 
	   (connected M11  aux   , sort lcolor(`Grape') msymbol("sh") lpattern(dash)  mcolor(`Grape')) ///
	   (rcap      M12 M13 aux ,  sort lcolor(`Grape')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Event Time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_05e.png", replace

*============================================================= 
*		6. education 
*============================================================= 
*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

event_margins rq t_event 1 tertiary "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_06.png", replace

*============================================================= 
*		7. assortative mating 
*============================================================= 
*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

event_margins rq t_event 1 assort_q "ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_07.png", replace


*============================================================= 
*		8. urban  
*============================================================= 
*----------------------------------------
* -. pooled
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 8	

event_margins rq t_event 1 urban_dv "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_08.png", replace
