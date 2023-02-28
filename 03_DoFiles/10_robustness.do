/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file carries out some robustness checks on the main result 
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


****************************************************
* using FE
****************************************************
use newparent_sample.dta, clear

keep if panel=="UKHLS"
xtset pidp wno

* drop events lags/leads with few obs
drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
local min = -`r(min)' + 1
replace t_event = t_event + `min'
local base = `min' - 1

* estimate model interacting t_event and gender 
xtreg rq ib`base'.t_event ${indiv_c} ${couple_c}  ib2.wno, fe vce(cluster cidp) nonest

tempfile event_study
parmest, saving(`event_study', replace)

** plot **
use `event_study', clear

egen event = seq() if strpos(parm, "t_event") > 0
drop if event == .
replace event = event - `min'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
	   text(0.3 3 "First child birth t=0", size(medium) just(left)) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   legend(off) 
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_fe.png", replace


****************************************************
* couples that don't split
****************************************************
use newparent_sample.dta, clear

* only couples who don't separate : 4,053 individuals 
ereplace separate = max(separate), by(cidp)
drop if separate==1

drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'


** Event-study regression **
reg rq ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* save regression results and use for plotting
tempfile event_study
parmest, saving(`event_study', replace)

** plot **
use `event_study', clear

egen event = seq() if strpos(parm, "t_event") > 0
drop if event == .
replace event = event - `min'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (rarea  ci_lb ci_ub event ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected coeff event, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
	   text(0.3 3 "First child birth t=0", size(medium) just(left)) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   legend(off) 
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_ns.png", replace

****************************************************
* total fertility 
****************************************************
use newparent_sample.dta, clear

replace total_kids = 3 if total_kids==4

* ensure a minimum amount of observations per bin
drop if t_event < -4 | t_event > 18

* recode factor variable
qui sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

forval xx = 1/3 {
	** Event-study regression **
	reg rq ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if total_kids==`xx', vce(cluster cidp)

	* save regression results and use for plotting
	tempfile event_study_`xx'
	parmest, saving(`event_study_`xx'', replace)
	
	preserve
	use `event_study_`xx'', clear
	
	* total children 
	gen tk = `xx'
	
	* event variable
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'

	save `event_study_`xx'', replace
	restore

}

use `event_study_1', clear
append using `event_study_2'
append using `event_study_3'


rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
forval xx = 1/3 {
	colorpalette lin fruits, locals
	twoway (rarea  ci_lb ci_ub event if tk==`xx',  sort color(${Tangerine}) fint(inten30) lw(none)) ///
		   (connected coeff event if tk==`xx', lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
		   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("Impact on RQ", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
		   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
		   legend(off) 
	graph display, ysize(5) xsize(5)
	graph export "${graphs}/Presentation/rq_tk`xx'.png", replace
}

****************************************************
* different baseline
****************************************************
use newparent_sample.dta, clear

drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 2
replace t_event = t_event + `min'


** Event-study regression **
reg rq ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* save regression results and use for plotting
tempfile event_study
parmest, saving(`event_study', replace)

** plot **
use `event_study', clear

egen event = seq() if strpos(parm, "t_event") > 0
drop if event == .
replace event = event - `min'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (rarea  ci_lb ci_ub event ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected coeff event, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
	   text(0.3 3 "First child birth t=0", size(medium) just(left)) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   legend(off) 
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_base.png", replace

****************************************************
* use a different rq measure 
****************************************************
*---------------------------
* factor subjective items
*---------------------------
use main_full_data.dta, clear

* factor 
factor ${items_subj} , pcf factors(5)
predict rq_subj*	// single factor
rename rq_subj1 rq_subj
label variable rq_subj "RQ (Subjective)"

* merge with couple identifiers
rename pidp f_pidp 
merge 1:n f_pidp panel wave using "couple_panel.dta", keep(1 3) nogen keepusing(cidp)
rename (f_pidp cidp) (m_pidp f_cidp)
merge 1:n m_pidp panel wave using "couple_panel.dta", keep(1 3) nogen keepusing(cidp)
rename m_pidp pidp

replace cidp = f_cidp if cidp==.
drop f_cidp

* keep only heterosexual couples 
drop if cidp==.

* new parents only 
sort ${unit}
keep if newparent==1

* set a minimum of 100 per bin
drop if t_event < -7 | t_event > 21	

* event study  
event_study rq_subj t_event 2 "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/rq_01_subj.png", replace


*---------------------------
* factor loadings of parents
*---------------------------
use main_full_data.dta, clear

* factor and save
keep if nchild>0
factor ${items}, pcf factors(5)
predict rq_parent*

matrix list r(scoef)
matrix F = r(scoef)[1..10, 1]

drop rq_parent2 
rename rq_parent1 rq_parent 

* open sample
use newparent_sample.dta, clear

* construct rq for all 
gen rq_parent = 0
forval xx = 1 (1) 10 {
	local var : word `xx' of ${items} 
	qui sum `var'
	replace rq_parent = rq_parent + ((`var' - `r(mean)')/`r(sd)')*F[`xx',1]
}
qui sum rq_parent
replace rq_parent = (rq_parent - `r(mean)')/`r(sd)'
label variable rq_parent "RQ (parent scores)"

* set a minimum of 100 per bin
drop if t_event < -7 | t_event > 21	

* event study  
event_study rq_parent t_event 2 "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/rq_01_parent.png", replace




//     ------------------------
//         Variable |  Factor1 
//     -------------+----------
//       screlpards |  0.16118 
//       screlparrg |  0.16868 
//       screlparar |  0.15308 
//       screlparir |  0.16075 
//       screlparwt |  0.15017 
//       screlparei |  0.15601 
//       screlparcd |  0.16881 
//       screlparks |  0.12870 
//      scparoutint |  0.11868 
//       screlhappy |  0.16246 
//     ------------------------


use main_full_data.dta, clear

replace event = 0 if event==1 & rq==.
egen aux = max(event), by(pidp)
keep if aux==1

keep if rq!=.
sort ${unit}

br pidp t_event

drop aux
gen aux = (t_event==-4 |t_event==-3 | t_event==-2 | t_event==-1)
ereplace aux = max(aux), by(pidp)
keep if aux==1
drop aux
gen aux = (t_event==3 | t_event==2 | t_event==1)
ereplace aux = max(aux), by(pidp)
keep if aux==1
drop aux
gen aux = (t_event==6 | t_event==5 | t_event==4)
ereplace aux = max(aux), by(pidp)
keep if aux==1

drop if t_event < -4 | t_event > 6	
event_study rq t_event 2 "${indiv_c} ${couple_c} ib2.wno" pidp "-1(.5).5"



****************************************************
* callaway and sant'anna  
****************************************************

use newparent_sample.dta, clear

keep if panel=="UKHLS"

* create treatment cohort variable 
gen cohort = event * wno
ereplace cohort = max(cohort), by(pidp)

keep if cohort>0

csdid rq , i(pidp) t(wno) g(cohort) method(dripw)

estat event

csdid_plot 

