clear all 
set more off 
if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}
cd "${samp}"


* these functions are to check stuff more quickly
do "${dofiles}/functions.do"

****************************************************
* 0. fertility rate
****************************************************
import delimited using "C:\Users\Olatz\Downloads\DP_LIVE_22012023133001280.csv", varnames(1) clear
keep if location=="GBR" | location=="EU" | location=="OAVG" 

twoway 	(connected value time if location=="OAVG", lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
		(connected value time if location=="EU"  , lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
		(connected value time if location=="GBR", lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")), ///
		ytitle("Fertility Rate", size(medsmall)) ylabel(#5, labsize(small)) ///
		xtitle("Year", size(medsmall)) xlabel(#6, labsize(small))   ///
		legend(order(1 "OECD" 2 "European Union" 3 "United Kingdom") pos(12) r(1) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/fertility.png", replace


****************************************************
* 1. main analysis
****************************************************
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

* no controls 
event_study rq t_event "" cidp
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_0.png", replace

* controls 
event_study rq t_event "${indiv_c} ${couple_c} ib2.wno" cidp
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_1.png", replace

//---------------------
// plot both
//---------------------
use newparent_sample.dta, clear

* drop events lags/leads with few obs
drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
local min = -`r(min)' + 1
replace t_event = t_event + `min'
local base = `min' - 1

* estimate model without controls 
reg rq ib`base'.t_event ,  vce(cluster cidp) 
tempfile event_study0
parmest, saving(`event_study0', replace)

* estimate model with controls 
reg rq ib`base'.t_event ${indiv_c} ${couple_c}  ib2.wno,  vce(cluster cidp) 
tempfile event_study
parmest, saving(`event_study', replace)

use `event_study0', clear
gen nc = 1 
append using `event_study'
replace nc = 0 if nc==.

egen event = seq() if strpos(parm, "t_event") > 0
drop if event == .
replace event = event - `min'
replace event = event - 29 if event>21

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (rarea  ci_lb ci_ub event if nc==1,  sort color(${Peach}) fint(inten30) lw(none)) ///
	   (rarea  ci_lb ci_ub event if nc==0,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected coeff event if nc==1, lcolor(`Peach') lpattern(dash_dot) mcolor(`Peach') msymbol("sh")) ///
	   (connected coeff event if nc==0, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
	   text(0.3 3 "First child birth t=0", size(medium) just(left)) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   legend(order(3 "Without controls" 4 "With controls") pos(12) r(1) size(medium)) 
graph display, ysize(5) xsize(9)
graph export "${graphs}/Presentation/rq_10.png", replace



****************************************************
* 2a. main analysis by gender 
****************************************************
use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 18

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate model interacting t_event and gender 
reg rq ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 

* marginal effects of t_event by gender
margins sex, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local m = 1
	local f = 1 + 1
	
	matrix e_male   = ( M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = ( M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local m = `xx'
	local f = `xx' + 1
	
	matrix e_male   = (e_male   \ M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = (e_female \ M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	
}

matrix M = (e_male , e_female )

svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea      M5 M6 aux ,  sort color(${Tangerine}) fint(inten30) lw(none)) /// 
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4  aux   , sort lcolor(${Tangerine}) msymbol("s") lpattern(dash)  mcolor(${Tangerine})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) ///
	   graphregion(fcolor(white)) ///
	   legend(order(3 "Fathers" 4 "Mothers" ) row(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_2a.png", replace

****************************************************
* 2b. main analysis significant gender 
****************************************************
use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 18

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate fully saturated model on gender
reg rq (ib`base'.t_event ${indiv_c} ${couple_c}  ib2.wno)##i.sex , vce(cluster cidp)

* store results in a matrix
matrix M = (_b[1.t_event#1.sex], _b[1.t_event#1.sex] - invttail(e(df_r),0.025)*_se[1.t_event#1.sex] , _b[1.t_event#1.sex] + invttail(e(df_r),0.025)*_se[1.t_event#1.sex] )	
qui sum t_event	
forval xx=2/`r(max)' {
	matrix M = (M \ _b[`xx'.t_event#1.sex], _b[`xx'.t_event#1.sex] - invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] , _b[`xx'.t_event#1.sex] + invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] )
	
}

* convert matrix into variables
svmat M		
egen aux = seq() if M1!=.
replace aux = aux - `min'

* plot
sum aux
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (rarea      M2 M3 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (connected M1  aux   , sort lcolor(`Blueberry') msymbol("sh") lpattern(dash)  mcolor(`Blueberry')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))    ///
	   legend(off)
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_2b.png", replace





****************************************************
* 3. general happiness 
****************************************************
use newparent_sample.dta, clear

* recode variable 
replace scghql = 4-scghql

* set a minimum of 100 per bin
drop if t_event < -7 | t_event > 21	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate model interacting t_event and gender 
reg scghql ib1.t_event ib1.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 

* marginal effects of t_event by gender
margins sex, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local m = 1
	local f = 1 + 1
	
	matrix e_male   = ( M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = ( M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local m = `xx'
	local f = `xx' + 1
	
	matrix e_male   = (e_male   \ M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = (e_female \ M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	
}

matrix M = (e_male , e_female )

svmat M	
* Columns: "Male" "Male, lb"  "Male, ub"  "Female"  "Female, lb"  "Female, ub"
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux ,  sort color(${Blueberry}) fintensity(inten30) lw(none)) ///
	   (rarea      M5 M6 aux ,  sort color(${Tangerine}) fintensity(inten30) lw(none)) /// 
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4  aux   , sort lcolor(${Tangerine}) msymbol("s") lpattern(dash)  mcolor(${Tangerine})), ///
	   xline(-.5, lpattern(dash) lcolor(gs11)) ///
	   ytitle("Impact on General Happiness", size(medsmall)) ylabel(-.6(.2).6, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) ///
	   legend(order(3 "Fathers" 4 "Mothers" ) row(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/wb_2a.png", replace



****************************************************
* 4. impact per item
****************************************************

use newparent_sample.dta, clear

* negative items negative 
foreach var in screlpards screlparrg screlparar screlparir {
	replace `var' = 5-`var' 
}

* drop events lags/leads with few obs
drop if t_event < -7 | t_event > 21

* recode factor variable		--> 	event is t_event==5
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* label variables 
local x = 0
foreach var in $items_subj $items_time {
	
	tempfile event_study_`var'
	
	local x = `x' + 1
	
	if `x'<6 {
		qui reg `var' ib`base'.t_event ${indiv_c} ${np_couple_c} ${items_time} ib2.wno, vce(cluster cidp)
	}
	else {
		qui reg `var' ib`base'.t_event ${indiv_c} ${np_couple_c} ${items_subj} ib2.wno, vce(cluster cidp)
	}

	* save regression results and use for plotting
	parmest, saving(`event_study_`var'', replace)

}
preserve 

local lablist  `" "consider splitting" "regret getting married" "quarrel" "get on each other's nerves" "degree of happiness" "work together on a project" "stimulating exchange of ideas" "calmly discuss something" "engage in outside interests" "kiss" "'
local x = 0
quietly {
foreach var in $items_subj $items_time {

	local x = `x'+1
	local labnum : word `x' of `lablist' 
	
	* plot 
	use `event_study_`var'', clear

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
		   (connected coeff event, lcolor(`Tangerine') lpattern(solid) mcolor(`Tangerine') msymbol("sh")), ///
		   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("", size(medsmall)) ylabel(-.75(.25).5, labsize(small)) yscale(outergap(*-3)) ///
		   xtitle("t", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) xscale(outergap(*-3)) ///
		   graphregion(fcolor(white)) ///
		   legend(off) ///
		   title("`labnum'", size(medlarge)) name(`var', replace) nodraw

}
}

foreach var in $items_subj $items_time {
	graph display `var' , ysize(5) xsize(5)
	graph export "${graphs}/Presentation/`var'.png", replace
}


****************************************************
* 5. housework
****************************************************

use newparent_sample.dta, clear

* drop events lags/leads with few obs
drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
local min = -`r(min)' + 1
replace t_event = t_event + `min'
local base = `min' - 1

* estimate model interacting t_event and gender 
reg howlng ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 

* marginal effects of t_event by gender
margins sex, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local m = 1
	local f = 1 + 1
	
	matrix e_male   = ( M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = ( M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local m = `xx'
	local f = `xx' + 1
	
	matrix e_male   = (e_male   \ M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_female = (e_female \ M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	
}

matrix M = (e_male , e_female )

svmat M	
* Columns: "Male" "Male, lb"  "Male, ub"  "Female"  "Female, lb"  "Female, ub"
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea      M5 M6 aux ,  sort color(${Tangerine}) fint(inten30) lw(none)) /// 
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4  aux   , sort lcolor(${Tangerine}) msymbol("s") lpattern(dash)  mcolor(${Tangerine})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Weekly Housework Hours", size(medsmall)) ylabel(#5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) ///
	   graphregion(fcolor(white)) ///
	   legend(order(3 "Fathers" 4 "Mothers" ) row(1) pos(north) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/hw_2a.png", replace


****************************************************
* 6. domestic labor change
****************************************************
use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 7

* recode factor variable		--> 	event is t_event==7
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.gbeh_q ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins gbeh_q, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 1
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
}

matrix M = (e_g1 , e_g2 )
svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* regress pooled 
reg gbeh ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gbeh_q!=., vce(cluster cidp)

* store marginal effects in matrix : linear, so same as coefficients
matrix list r(table)
matrix M0 = r(table)

matrix P = ( M0[1,1] , M0[5,1] , M0[6,1] )	

qui sum t_event	
forval xx=2(1)`r(max)' {
	matrix P   = (P \ M0[1,`xx'] , M0[5,`xx'] , M0[6,`xx'] )	
}

svmat P

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  P2 P3 aux,  sort color(gs10%30) fint(inten30) lw(0)) ///
	   (rarea  M2 M3 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M5 M6 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected P1 aux, lcolor(gs10%50) lpattern(dash_dot) mcolor(gs10%50) msymbol("o")) ///
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Domesitic Labor Division", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(4 "Pooled" 5 "Traditional" 6 "Egalitarian") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/gbeh_1d.png", replace


****************************************************
* 7. domestic labor on rq
****************************************************
use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 8	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* regress
reg rq ib`base'.t_event ib`base'.t_event#i.gbeh_q ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins gbeh_q, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 1
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
}

matrix M = (e_g1 , e_g2 )
svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* regress pooled 
reg gbeh ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gbeh_q!=., vce(cluster cidp)

* store marginal effects in matrix : linear, so same as coefficients
matrix list r(table)
matrix M0 = r(table)

matrix P = ( M0[1,1] , M0[5,1] , M0[6,1] )	

qui sum t_event	
forval xx=2(1)`r(max)' {
	matrix P   = (P \ M0[1,`xx'] , M0[5,`xx'] , M0[6,`xx'] )	
}

svmat P

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M5 M6 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(3 "Traditional" 4 "Egalitarian") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_1d.png", replace



****************************************************
* education on rq
****************************************************
use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 10

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg rq ib`base'.t_event#i.tertiary ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins tertiary, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 3
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
}

matrix M`qt' = (e_g1 , e_g2 )
svmat M`qt'	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux,  sort color(${Apple}) fint(inten30) lw(none)) ///
	   (rarea M5 M6 aux,  sort color(${Grape}) fint(inten30) lw(none)) ///
	   (connected M1 aux, lcolor(${Apple}) lpattern(dash) mcolor(${Apple}) msymbol("sh")) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash) mcolor(${Grape}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(3 "Non-college" 4 "College") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_1e.png", replace



****************************************************
* assort on rq
****************************************************
use newparent_sample.dta, clear

recode assort (3 = 2) (4 = 3)
* 1. event study
tab t_event assort if rq!=.

* set minimum bn size 
drop if t_event < -3 | t_event > 7

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate interacted model 
reg rq ib`base'.t_event ib`base'.t_event#ib1.assort ${indiv_c} ${np_couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by educational level
margins, dydx(t_event) at(assort=(1 2 3)) 

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local l  = 1
	local ml = 1 + 1
	local mh = 1 + 2
	
	matrix e_low  = ( M0[1,`l'] , M0[5,`l'] , M0[6,`l'] )	
	matrix e_ml = ( M0[1,`ml'] , M0[5,`ml'] , M0[6,`ml'] )	
	matrix e_mh = ( M0[1,`mh'] , M0[5,`mh'] , M0[6,`mh'] )	

	local max = `r(max)'*3 - 2
	
forval xx=4(3)`max' {
	local l  = `xx'
	local ml = `xx' + 1
	local mh = `xx' + 2
	
	matrix e_low  = (e_low  \ M0[1,`l'] , M0[5,`l'] , M0[6,`l'] )	
	matrix e_ml = ( e_ml \ M0[1,`ml'] , M0[5,`ml'] , M0[6,`ml'] )	
	matrix e_mh = ( e_mh \ M0[1,`mh'] , M0[5,`mh'] , M0[6,`mh'] )	
}

matrix M = (e_low, e_ml , e_mh )

svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M2 M3 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M8 M9 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M7 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Non-college" 4 "College") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_assort.png", replace




*********************************************
*********************************************
*				appendix					*
*********************************************
*********************************************


****************************************************
* distribution of RQ
****************************************************
use newparent_sample.dta, clear

sum rq if sex==0
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==0, bin(40) color(${Blueberry}) ///
	ytitle("density", size(medium)) ylabel(0(.2).6, labs(medsmall))  yscale(outergap(*-3)) ///
	xtitle("RQ", size(medium)) xlabel(, labs(medsmall))  xscale(outergap(*-3)) ///
	text(0.5 -5 "Mean: 0.04" "SD: 0.95", size(medium) just(left))
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/dist_m.png", replace
	
qui sum rq if sex==1
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==1, bin(40) color(${Tangerine}) ///
	ytitle("density", size(medium)) ylabel(0(.2).6, labs(medsmall))  yscale(outergap(*-3)) ///
	xtitle("RQ", size(medium)) xlabel(, labs(medsmall))  xscale(outergap(*-3)) ///
	text(0.5 -5 "Mean: -0.05" "SD: `sd'", size(medium) just(left))
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/dist_w.png", replace



****************************************************
* smooth evolution
****************************************************
use full_sample.dta, clear  
keep if age>=25 & age<=70
keep if tenure<=40
xtreg rq ${indiv_c} ${couple_c} ib2.wno, fe vce(cluster cidp) nonest

colorpalette lin fruits, local 
coefplot, keep(*age) vertical recast(connected) msymbol("sh") lpattern(solid) lc(`Apple') mc(${Apple}) noci ///
		coeflabel(26.age="26" 27.age=" " 28.age=" " 29.age=" " 30.age="30" 31.age=" " 32.age=" " 33.age=" " ///
				  34.age=" " 35.age="35" 36.age=" " 37.age=" " 38.age=" " 39.age=" " 40.age="40" 41.age=" " ///
				  42.age=" " 43.age=" " 44.age=" " 45.age="45" 46.age=" " 47.age=" " 48.age=" " 49.age=" "  ///
				  50.age="50" 51.age=" " 52.age=" " 53.age=" " 54.age=" " 55.age="55" 56.age=" " 57.age=" " ///
				  58.age=" " 59.age=" " 60.age="60" 61.age=" " 62.age=" " 63.age=" " 64.age=" " 65.age="65" ///
				  66.age=" " 67.age=" " 68.age=" " 69.age=" " 70.age="70" , labsize(small)) ///
		yline(0) ylabel(-1.6(.4)1.6,labsize(small)) ytitle("Age Profile", size(medsmall)) yscale(titlegap(*-20) outergap(*-3)) ///
		xtitle("Age", size(medsmall)) xscale(outergap(*-3)) ///
	    legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/age.png", replace
		
colorpalette lin fruits, local 
coefplot, keep(*tenure) vertical recast(connected) msymbol("sh") lpattern(solid) lc(`Apple') mc(${Apple}) noci ///
		yline(0) ylabel(-1.6(.4)1.6,labsize(small)) ytitle("Tenure Profile", size(medsmall)) yscale(titlegap(*-20) outergap(*-3))  ///
		xlabel(#10,labsize(small)) xtitle("Tenure", size(medsmall)) xscale(outergap(*-3)) ///
		legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/tenure.png", replace



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
* main analysis gender margins 
****************************************************
use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 18

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate model interacting t_event and gender 
reg rq ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 

* marginal effects of t_event by gender
margins t_event#sex
marginsplot, plot1opts(lcolor(${Blueberry}) msymbol("sh") lpattern(dash)  mcolor(${Blueberry})) /// 
   plot2opts(lcolor(${Tangerine}) msymbol("sh") lpattern(dash)  mcolor(${Tangerine})) ///
   ci1opts(recast(rarea) color(${Blueberry}) fint(inten30) lw(none)) ///
   ci2opts(recast(rarea) color(${Tangerine}) fint(inten30) lw(none)) ///
   xline(4.5, lpattern(dash) lcolor(gs11)) ///
   ytitle("Predictive margins", size(medsmall)) ylabel(#5, labsize(small)) ///
   xtitle("Years around first child birth", size(medsmall)) ///
   xlabel(1  "-4" 3  "-2" 5 "0" 7  "2" 9 "4" 11 "6" 13 "8" 15 "10" 17 "12" 19 "14" 21 "16" 23 "18", labsize(small))    ///
   title("", size(tiny)) ///
   legend(order(3 "Fathers" 4 "Mothers") row(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_2c.png", replace

****************************************************
* domestic labor on housework hours
****************************************************
use newparent_sample.dta, clear

gen group = 1 if sex==0 & gbeh_q==1
replace group = 2 if sex==0 & gbeh_q==2
replace group = 3 if sex==1 & gbeh_q==1
replace group = 4 if sex==1 & gbeh_q==2

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 8	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* regress
reg howlng ib`base'.t_event ib`base'.t_event#i.group ib25.age i.tertiary i.employ c.lincome i.urban_dv ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins t_event#group
marginsplot, plot1opts(lcolor(${Grape}) msymbol("sh") lpattern(dash)  mcolor(${Grape})) /// 
   plot2opts(lcolor(${Apple}) msymbol("sh") lpattern(dash)  mcolor(${Apple})) ///
   plot3opts(lcolor(${Blueberry}) msymbol("sh") lpattern(dash)  mcolor(${Blueberry})) ///
   plot4opts(lcolor(${Tangerine}) msymbol("sh") lpattern(dash)  mcolor(${Tangerine})) ///
   ci1opts(recast(rarea) color(${Grape}) fint(inten30) lw(none)) ///
   ci2opts(recast(rarea) color(${Apple}) fint(inten30) lw(none)) ///
   ci3opts(recast(rarea) color(${Blueberry}) fint(inten30) lw(none)) ///
   ci4opts(recast(rarea) color(${Tangerine}) fint(inten30) lw(none)) ///
   xline(4.5, lpattern(dash) lcolor(gs11)) ///
   ytitle("Predictive margins", size(medsmall)) ylabel(#5, labsize(small)) ///
   xtitle("Years around first child birth", size(medsmall)) ///
   xlabel(1  "-4" 3  "-2" 5 "0" 7  "2" 9 "4" 11 "6" 13 "8", labsize(small))    ///
   title("", size(tiny)) ///
   legend(order(5 "Traditional Fathers" 6 "Egalitarian Fathers" 7 "Traditional Mothers" 8 "Egalitarian Mothers") row(2) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/hw_1d.png", replace

****************************************************
* child sex on rq
****************************************************

use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 12

event_margins rq t_event ch_sex "${indiv_c} ${couple_c}" cidp
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_1g.png", replace


****************************************************
* education on housework
****************************************************
use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 10

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* separately by group 
qui levelsof tertiary, local(sep)
foreach qt of local sep {
	* regress interacting t_event with group
	reg howlng ib`base'.t_event#i.sex ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno if tertiary==`qt', vce(cluster cidp)

	* marginal effects of t_event by gnorm category
	margins sex, dydx(t_event)

	* store effects in matrix
	matrix list r(table)
	matrix M0 = r(table)

	qui sum t_event	
		local g1 = 1
		local g2 = 1 + 3
		
		matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
		matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

		local max = `r(max)'*2 - 1
		
	forval xx=3(2)`max' {
		local g1 = `xx'
		local g2 = `xx' + 1
		
		matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
		matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
	}

	matrix M`qt' = (e_g1 , e_g2 )
	svmat M`qt'	
}
 
egen aux = seq() if M11!=.
replace aux = aux - `min'

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M02 M03 aux,  sort color(${Grape}) fint(inten30) lw(none)) ///
	   (rarea M05 M06 aux,  sort color(${Apple}) fint(inten30) lw(none)) ///
	   (rarea  M12 M13 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M15 M16 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M01 aux, lcolor(${Grape}) lpattern(dash) mcolor(${Grape}) msymbol("sh")) ///
	   (connected M04 aux, lcolor(${Apple}) lpattern(dash) mcolor(${Apple}) msymbol("sh")) ///
	   (connected M11 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M14 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Weekly Housework Hours", size(medsmall)) ylabel(#5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(5 "No-college Fathers" 6 "No-college Mothers" 7 "College Fathers" 8 "College Mothers") r(2) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/hw_1e.png", replace


****************************************************
* education on ddl
****************************************************
use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 10

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

gen egroup = 1 if tertiary==0 & gbeh_q==1
replace egroup = 2 if tertiary==0 & gbeh_q==2
replace egroup = 3 if tertiary==1 & gbeh_q==1
replace egroup = 4 if tertiary==1 & gbeh_q==2

reg gbeh ib`base'.t_event#ib1.gbeh_q ib25.age i.employ c.lincome i.urban_dv ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins gbeh_q, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 1
// 	local g3 = 1 + 2
// 	local g4 = 1 + 3
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
// 	matrix e_g3 = ( M0[1,`g3'] , M0[5,`g3'] , M0[6,`g3'] )	
// 	matrix e_g4 = ( M0[1,`g4'] , M0[5,`g4'] , M0[6,`g4'] )	

// 	local max = `r(max)'*4- 3
	local max = `r(max)'*2- 1

	
// forval xx=5(4)`max' {
forval xx=3(2)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
// 	local g3 = `xx' + 2
// 	local g4 = `xx' + 3
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
// 	matrix e_g3   = (e_g3 \ M0[1,`g3'] , M0[5,`g3'] , M0[6,`g3'] )	
// 	matrix e_g4   = (e_g4 \ M0[1,`g4'] , M0[5,`g4'] , M0[6,`g4'] )	
}

// matrix M`qt' = (e_g1 , e_g2 , e_g3 , e_g4)
matrix M = (e_g1 , e_g2)
svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
// twoway (rarea  M2 M3 aux,  sort color(${Grape}) fint(inten30) lw(none)) ///
// 	   (rarea M5 M6 aux,  sort color(${Apple}) fint(inten30) lw(none)) ///
// 	   (rarea  M8 M9 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
// 	   (rarea M11 M12 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
// 	   (connected M1 aux, lcolor(${Grape}) lpattern(dash) mcolor(${Grape}) msymbol("sh")) ///
// 	   (connected M4 aux, lcolor(${Apple}) lpattern(dash) mcolor(${Apple}) msymbol("sh")) ///
// 	   (connected M7 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
// 	   (connected M10 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
// 	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
// 	   ytitle("Impact on Domestic Labor Division", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
// 	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
// 	   legend(order(5 "No-college Traditional" 6 "No-college Egalitarian" 7 "College Traditional" 8 "College Egalitarian") r(2) pos(12) size(medsmall)) 
twoway (rarea  M2 M3 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M5 M6 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Domestic Labor Division", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "No-college" 4 "College") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/gbeh_1e.png", replace

tab gbeh_q tertiary

****************************************************
* assort on happiness
****************************************************
use newparent_sample.dta, clear
recode assort (3 = 2) (4 = 3)

* 1. event study
tab t_event assort if scghql!=. & sex==1
replace scghql = 4-scghql


* set minimum bn size 
drop if t_event < -4 | t_event > 8

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 2
replace t_event = t_event + `min'

* by sex 
forval sx=0/1 {
	* estimate interacted model 
	reg scghql ib`base'.t_event ib`base'.t_event#ib1.assort ib25.age i.tertiary i.employ c.lincome i.urban_dv ${np_couple_c} ib2.wno if sex==`sx', vce(cluster cidp)

	* marginal effects of t_event by educational level
	margins, dydx(t_event) at(assort=(1 2 3)) 

	* store effects in matrix
	matrix list r(table)
	matrix M0 = r(table)

	qui sum t_event	
		local l  = 1
		local ml = 1 + 1
		local mh = 1 + 2
		
		matrix e_low  = ( M0[1,`l'] , M0[5,`l'] , M0[6,`l'] )	
		matrix e_ml = ( M0[1,`ml'] , M0[5,`ml'] , M0[6,`ml'] )	
		matrix e_mh = ( M0[1,`mh'] , M0[5,`mh'] , M0[6,`mh'] )	

		local max = `r(max)'*3 - 2
		
	forval xx=4(3)`max' {
		local l  = `xx'
		local ml = `xx' + 1
		local mh = `xx' + 2
		
		matrix e_low  = (e_low  \ M0[1,`l'] , M0[5,`l'] , M0[6,`l'] )	
		matrix e_ml = ( e_ml \ M0[1,`ml'] , M0[5,`ml'] , M0[6,`ml'] )	
		matrix e_mh = ( e_mh \ M0[1,`mh'] , M0[5,`mh'] , M0[6,`mh'] )	
	}

	matrix M`sx' = (e_low, e_ml , e_mh )
	svmat M`sx'	

}

 
egen aux = seq() if M11!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M12 M13 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M18 M19 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M11 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M17 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on General Happiness", size(medsmall)) ylabel(-.6(.2).6, labsize(small))  xscale(outergap(*-3)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) yscale(outergap(*-3) titlegap(*-20))  ///
	   legend(order(3 "No college" 4 "College") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_1f_w.png", replace

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M02 M03 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M08 M09 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M01 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M07 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on General Happiness", size(medsmall)) ylabel(-.6(.2).6, labsize(small)) yscale(outergap(*-3) titlegap(*-20)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) xscale(outergap(*-3)) ///
	   legend(order(3 "No college" 4 "College") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/Presentation/rq_1f_m.png", replace


