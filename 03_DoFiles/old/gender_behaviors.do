clear all 
set more off 


if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"



***************************************************************************
*	GENDER BEHAVIORS: Domestic Division of Labor module
*
* 		0. Describe percentage responses to each item + index 
* 		1. Event study using division of labor as an outcome
*			1.1. Pooled
*			1.2. Gender differences 
*			1.3. Dividing into gender norm categories
*			1.4. Dividing into gender behavior categories 
*			1.5. Dividing into child sex
*			1.6. Dividing them into assortativeness
*		2. Event study on RQ dividing into gbeh categories
*			2.1. Pooled
*			2.2. Category differences 
*			2.3. Gender differences 
***************************************************************************

*============================================================================
*	0. Describe individual responses to each task
*============================================================================

use newparent_sample.dta, clear

* traditionally male 
foreach var of varlist huboss hudiy hupots {	
	* traditional hh
	gen sh_`var'_trad = (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace sh_`var'_trad = . if `var'==.

	* counter traditional hh
	gen sh_`var'_count = (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace sh_`var'_count = . if `var'==.

	* egalitarian hh
	gen sh_`var'_egal = `var'==3
	replace sh_`var'_egal = . if `var'==.
}

* traditionally female 
foreach var of varlist hubuys hufrys huiron humops husits_dv {	
	* traditional hh
	gen sh_`var'_trad = (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace sh_`var'_trad = . if `var'==.

	* counter traditional hh
	gen sh_`var'_count = (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace sh_`var'_count = . if `var'==.

	* egalitarian hh
	gen sh_`var'_egal = `var'==3
	replace sh_`var'_egal = . if `var'==.
}
	
label variable sh_huboss_egal "household financial decisions"
label variable sh_hubuys_egal "who does the grocery shopping"
label variable sh_hudiy_egal "who does the diy jobs"
label variable sh_hufrys_egal "who does the cooking"
label variable sh_huiron_egal "who does the washing/ironing"
label variable sh_humops_egal "who does the cleaning"
label variable sh_hupots_egal "who does the gardening"
label variable sh_husits_dv_egal "who is responsible for childcare"

eststo trad_gbeh: 	estpost summarize *_trad
eststo egal_gbeh:   estpost summarize *_egal
eststo count_gbeh:  estpost summarize *_count

use "newparent_couples.dta", clear
eststo agree_gbeh:	estpost summarize *_ag


esttab *_gbeh using "${tabs}/gbeh.tex", ///
	rename(sh_huboss_trad sh_huboss_egal sh_hubuys_trad sh_hubuys_egal sh_hudiy_trad sh_hudiy_egal sh_hufrys_trad sh_hufrys_egal sh_huiron_trad sh_huiron_egal sh_humops_trad sh_humops_egal sh_hupots_trad sh_hupots_egal sh_husits_dv_trad sh_husits_dv_egal sh_huboss_count sh_huboss_egal sh_hubuys_count sh_hubuys_egal sh_hudiy_count sh_hudiy_egal sh_hufrys_count sh_hufrys_egal sh_huiron_count sh_huiron_egal sh_humops_count sh_humops_egal sh_hupots_count sh_hupots_egal sh_husits_dv_count sh_husits_dv_egal huboss_ag sh_huboss_egal hubuys_ag sh_hubuys_egal hufrys_ag sh_hufrys_egal huiron_ag sh_huiron_egal humops_ag sh_humops_egal hudiy_ag sh_hudiy_egal hupots_ag sh_hupots_egal husits_dv_ag sh_husits_dv_egal ) ///
	mtitles("traditional" "egalitarian" "counter- trad." "agree") ///
	main(mean) aux(sd) label replace


*============================================================================
* 	1. Event study using division of labor index as an outcome
*============================================================================

*-----------------------------------------------------------------------
*	1.1. Pooled regression
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -6 | t_event > 20

* recode factor variable		--> 	event is t_event==7
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress
reg gbeh ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* save regression results 
tempfile event_study
parmest, saving(`event_study', replace)

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
twoway (connected coeff event, lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event ,  sort lcolor(`Cherry')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1a.png", replace


*-----------------------------------------------------------------------
*	1.2. Gender differences 
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -6 | t_event > 20

* recode factor variable		--> 	event is t_event==7
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress: fully saturated model
reg gbeh (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex , vce(cluster cidp)

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
twoway (connected M1  aux   , sort lcolor(`Blueberry') msymbol("sh") lpattern(dash)  mcolor(`Blueberry')) ///
	   (rcap      M2 M3 aux ,  sort lcolor(`Blueberry')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Differential impact on mothers relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))    ///
	   legend(off)
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1b.png", replace


*-----------------------------------------------------------------------
*	EXTRA: marginal effects split by gender 
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -6 | t_event > 20

* recode factor variable		--> 	event is t_event==7
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
* Columns: "Male" "Male, lb"  "Male, ub"  "Female"  "Female, lb"  "Female, ub"
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'

colorpalette lin fruits, global opacity(50)
twoway (connected M1 aux, lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
	   (rcap  M2 M3 aux ,  sort lcolor(${Blueberry})) ///
	   (connected M4  aux   , sort lcolor(${Cherry}) msymbol("s") lpattern(dash)  mcolor(${Cherry})) ///
	   (rcap      M5 M6 aux ,  sort lcolor(${Cherry})), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Marginal effect of parenthood on RQ", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Fathers" 3 "Mothers" ) row(1) pos(north) size(medsmall)) 


*-----------------------------------------------------------------------
*	1.3. Gender norm differences 
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

* set a minimum of 80 per bin
drop if t_event < -4 | t_event > 7	

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.gnorm_q ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins gnorm_q, dydx(t_event)

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
reg gbeh ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gnorm_q!=., vce(cluster cidp)

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
twoway (connected P1 aux, lcolor(gs10%50) lpattern(dash_dot) mcolor(gs10%50) msymbol("sh")) ///
	   (rarea  P2 P3 aux,  sort color(gs10%30) lw(0)) ///
	   (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Pooled" 3 "Conservative" 5 "Progressive") r(1) pos(12) size(medsmall)) 
*graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1c.png", replace


*-----------------------------------------------------------------------
*	1.4. Gender behavior categories
*-----------------------------------------------------------------------

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
twoway (connected P1 aux, lcolor(gs10%50) lpattern(dash_dot) mcolor(gs10%50) msymbol("sh")) ///
	   (rarea  P2 P3 aux,  sort color(gs10%30) lw(0)) ///
	   (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Pooled" 3 "Traditional" 5 "Egalitarian") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1d.png", replace


*-----------------------------------------------------------------------
*	1.5. Child sex
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 11

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.ch_sex ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins ch_sex, dydx(t_event)

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

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1e.png", replace

*-----------------------------------------------------------------------
*	1.6. Assortative mating
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 11

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.assort ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins assort, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 3
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

	local max = `r(max)'*2 - 1
	
forval xx=5(4)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
}

matrix M = (e_g1 , e_g2 )
svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1e.png", replace


*-----------------------------------------------------------------------
*	EXTRA: Child sex, only men
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 11

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.ch_sex ${indiv_c} ${couple_c} ib2.wno if sex==0, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins ch_sex, dydx(t_event)

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

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
*graph export "${graphs}/gbeh_1e.png", replace


*-----------------------------------------------------------------------
*	1.6. Assortative mating
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

drop if t_event < -4 | t_event > 9

gen assort_g = 1 if assort==4
replace assort_g = 0 if assort==1
replace assort_g = 2 if assort==2 | assort==3

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress interacting t_event with group
reg gbeh ib`base'.t_event#i.assort_g ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* marginal effects of t_event by gnorm category
margins assort_g, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local g1 = 1
	local g2 = 1 + 1
	local g3 = 1 + 2
	
	matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
	matrix e_g3 = ( M0[1,`g3'] , M0[5,`g3'] , M0[6,`g3'] )	

	local max = `r(max)'*3 - 2
	
forval xx=4(3)`max' {
	local g1 = `xx'
	local g2 = `xx' + 1
	local g3 = `xx' + 2
	
	matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
	matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
	matrix e_g3   = (e_g3 \ M0[1,`g3'] , M0[5,`g3'] , M0[6,`g3'] )	
}

matrix M = (e_g1 , e_g2 , e_g3 )
svmat M	
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

* plot all
sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux,  sort lcolor(${Tangerine})) ///
	   (connected M4 aux, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap M5 M6 aux,  sort lcolor(${Grape})) ///
	   (connected M7 aux, lcolor(${Apple}) lpattern(dash_dot) mcolor(${Apple}) msymbol("sh")) ///
	   (rcap M8 M9 aux,  sort lcolor(${Apple})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1.5(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(1 "Assort. low" 3 "Not assort." 5 "Assort. high") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_1f.png", replace




*============================================================================
* 	2. Event Study on RQ using gbeh categories
*============================================================================

use newparent_sample.dta, clear
tab t_event if gbeh_q!=. & rq!=.

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 8	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'


*-----------------------------------------------------------------------
*	2.1. Pooled  
*-----------------------------------------------------------------------
qui levelsof gbeh_q, local(quintile) 
foreach qt of local quintile {
	preserve 
	keep if gbeh_q == `qt'
	
	* regress 
	reg rq ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
	
	* save results
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	
	* transform results file 
	use `event_study`qt'', clear
	
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	
	gen qtile = `qt'
	
	save `event_study`qt'', replace

	restore
}

preserve 

use `event_study1', clear
append using `event_study2'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1, lcolor(`Peach') lpattern(dash_dot) mcolor(`Peach') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1,  sort lcolor(`Peach')) ///
	   (connected coeff event if qtile==2, lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2,  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Traditional" 3 "Egalitarian") row(1) pos(north) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_2a.png", replace

restore 

*-----------------------------------------------------------------------
*	2.2. Significant differences 
*-----------------------------------------------------------------------

preserve
keep if gbeh_q==1 | gbeh_q==2

reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##ib1.gbeh_q, vce(cluster cidp)

* store results in a matrix
matrix M = (_b[1.t_event#2.gbeh_q], _b[1.t_event#2.gbeh_q] - invttail(e(df_r),0.025)*_se[1.t_event#2.gbeh_q] , _b[1.t_event#2.gbeh_q] + invttail(e(df_r),0.025)*_se[1.t_event#2.gbeh_q] )	
qui sum t_event	
forval xx=2/`r(max)' {
	matrix M = (M \ _b[`xx'.t_event#2.gbeh_q], _b[`xx'.t_event#2.gbeh_q] - invttail(e(df_r),0.025)*_se[`xx'.t_event#2.gbeh_q] , _b[`xx'.t_event#2.gbeh_q] + invttail(e(df_r),0.025)*_se[`xx'.t_event#2.gbeh_q] )
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
twoway (connected M1  aux   , sort lcolor(`Blueberry') msymbol("sh") lpattern(dash)  mcolor(`Blueberry')) ///
	   (rcap      M2 M3 aux ,  sort lcolor(`Blueberry')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Differential impact on egalitarian relative to t=-1", size(medsmall)) ylabel(#5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(off)
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_2b.png", replace

restore

replace t_event = t_event - `min'




*-----------------------------------------------------------------------
*	2.3. Gender differences 
*-----------------------------------------------------------------------

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 6	

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

foreach qt in 1 2 {
	reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if gbeh_q==`qt', vce(cluster cidp)

	* store results in a matrix
	matrix M`qt' = (_b[1.t_event#1.sex], _b[1.t_event#1.sex] - invttail(e(df_r),0.025)*_se[1.t_event#1.sex] , _b[1.t_event#1.sex] + invttail(e(df_r),0.025)*_se[1.t_event#1.sex] )	
	qui sum t_event	
	forval xx=2/`r(max)' {
		matrix M`qt' = (M`qt' \ _b[`xx'.t_event#1.sex], _b[`xx'.t_event#1.sex] - invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] , _b[`xx'.t_event#1.sex] + invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] )
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
	   ytitle("Differential impact on mothers relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Traditional" 3 "Progressive") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_2c.png", replace


***************************************************************************
*	GENDER BEHAVIOR AGREEMENT: 
*
* 		1. Event study using gender behavior agreement as an outcome 
*			1.1. Pooled
*			1.2. Gender differences 
*			1.3. Dividing into gender norm categories
*			1.4. Dividing into gender behavior categories 
*			1.5. Dividing into child sex

***************************************************************************
use newparent_sample.dta, clear
*tab t_event if gbeh_ag!=.

* set a minimum of 100 per bin
drop if t_event < -6 | t_event > 20

* recode factor variable		--> 	event is t_event==7
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'


*-----------------------------------------------------------------------
*	1.1. Pooled regression
*-----------------------------------------------------------------------
reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* save regression results 
tempfile event_study
parmest, saving(`event_study', replace)

preserve 

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
twoway (connected coeff event, lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event ,  sort lcolor(`Cherry')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Agreement relative to t=-1", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_1a.png", replace

restore 


*-----------------------------------------------------------------------
*	1.2. Gender differences 
*-----------------------------------------------------------------------
reg gbeh_ag (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex , vce(cluster cidp)

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
twoway (connected M1  aux   , sort lcolor(`Blueberry') msymbol("sh") lpattern(dash)  mcolor(`Blueberry')) ///
	   (rcap      M2 M3 aux ,  sort lcolor(`Blueberry')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Differential impact on mothers relative to t=-1", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small))    ///
	   legend(off)
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_1b.png", replace

drop aux
replace t_event = t_event - `min'


*-----------------------------------------------------------------------
*	1.3. Gender norm categories 
*-----------------------------------------------------------------------
tab t_event gnorm_q if gbeh_ag!=. 

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 7	

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* regress for each level
qui levelsof gnorm_q, local(quintile) 
foreach qt of local quintile {
	preserve 

	* regress 
	reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gnorm_q==`qt', vce(cluster cidp)
	
	* save results
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	
	* transform results file 
	use `event_study`qt'', clear
	
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	
	gen qtile = `qt'
	
	save `event_study`qt'', replace

	restore	
}

preserve 

use `event_study1', clear
append using `event_study2'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1,  sort lcolor(${Tangerine})) ///
	   (connected coeff event if qtile==2, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Agreement relative to t=-1", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(1 "Conservative" 3 "Progressive") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_1c.png", replace

restore 


*-----------------------------------------------------------------------
*	1.4. Gender behavior categories  
*-----------------------------------------------------------------------

* regress each group 
levelsof gbeh_q, local(quar)
foreach qt of local quar {
	reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gbeh_q==`qt', vce(cluster cidp)
	* save
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	* modify 
	preserve 
	use `event_study`qt'', clear 
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	gen qt = `qt'
	save `event_study`qt'', replace
	restore 
}
* regress pooled 
	reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gbeh_q!=., vce(cluster cidp)
	* save
	tempfile event_study
	parmest, saving(`event_study', replace)
	* modify 
	preserve 
	use `event_study', clear 
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	gen qt = 0
	save `event_study', replace
	restore 

preserve 

use `event_study', clear
append using `event_study1'
append using `event_study2'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
twoway (connected coeff event if qt==0, lcolor(gs10%50) lpattern(dash_dot) mcolor(gs10%50) msymbol("sh")) ///
	   (rarea  ci_lb ci_ub event if qt==0 ,  sort color(gs10%30) lw(0)) ///
	   (connected coeff event if qt==1, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qt==1 ,  sort lcolor(${Tangerine})) ///
	   (connected coeff event if qt==2, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qt==2 ,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Agreement relative to t=1", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Pooled" 3 "Traditional" 5 "Egalitarian") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_1d.png", replace

restore


*-----------------------------------------------------------------------
*	1.5. Child sex
*-----------------------------------------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 11
* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'


qui levelsof ch_sex, local(quintile) 
foreach qt of local quintile {
	preserve 

	* regress 
	reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if ch_sex==`qt', vce(cluster cidp)
	
	* save results
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	
	* transform results file 
	use `event_study`qt'', clear
	
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	
	gen qtile = `qt'
	
	save `event_study`qt'', replace

	restore	
}

preserve 

use `event_study0', clear
append using `event_study1'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==0, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==0,  sort lcolor(${Tangerine})) ///
	   (connected coeff event if qtile==1, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Agreement relative to t=-1", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_1e.png", replace

restore 

*-----------------------------------------------------------------------
*	1.7. Assortative mating
*-----------------------------------------------------------------------
use newparent_sample.dta, clear

tab t_event assort if gbeh_ag!=.

drop if t_event < -4 | t_event > 8

gen assort_g = 1 if assort==4
replace assort_g = 0 if assort==1
replace assort_g = 2 if assort==2 | assort==3

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'


qui levelsof assort_g, local(quintile) 
foreach qt of local quintile {
	preserve 

	* regress 
	reg gbeh_ag ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if assort_g==`qt', vce(cluster cidp)
	
	* save results
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	
	* transform results file 
	use `event_study`qt'', clear
	
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	
	gen qtile = `qt'
	
	save `event_study`qt'', replace

	restore	
}

preserve 

use `event_study0', clear
append using `event_study1'
append using `event_study2'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==0, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==0,  sort lcolor(${Tangerine})) ///
	   (connected coeff event if qtile==2, lcolor(${Apple}) lpattern(dash_dot) mcolor(${Apple}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2,  sort lcolor(${Apple})) ///
	   (connected coeff event if qtile==1, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Agreement relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Assort. low" 3 "Not assort." 5 "Assort. high") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
*graph export "${graphs}/gbagree_1f.png", replace

restore 


***************************************************************************
*	CHLD SEX
*
*		2. Event study on RQ dividing into gbeh categories
*			2.1. Pooled
*			2.3. Gender differences 
***************************************************************************

use newparent_sample.dta, clear

tab t_event if ch_sex!=. & rq!=.
drop if t_event < -7 | t_event > 21

* recode factor variable
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

*-----------------------------------------------------------------------
*	2.1. Pooled
*-----------------------------------------------------------------------

qui levelsof ch_sex, local(quintile) 
foreach qt of local quintile {
	preserve 

	* regress 
	reg rq ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if ch_sex==`qt', vce(cluster cidp)
	
	* save results
	tempfile event_study`qt'
	parmest, saving(`event_study`qt'', replace)
	
	* transform results file 
	use `event_study`qt'', clear
	
	egen event = seq() if strpos(parm, "t_event") > 0
	drop if event == .
	replace event = event - `min'
	
	gen qtile = `qt'
	
	save `event_study`qt'', replace

	restore	
}


use `event_study0', clear
append using `event_study1'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==0, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==0,  sort lcolor(${Tangerine})) ///
	   (connected coeff event if qtile==1, lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1,  sort lcolor(${Grape})), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/chsex_2a.png", replace



*-----------------------------------------------------------------------
*	2.3. Gender differences 
*-----------------------------------------------------------------------

use newparent_sample.dta, clear

tab t_event sex if ch_sex==0 & rq!=.
tab t_event sex if ch_sex==1 & rq!=.

* set a minimum of 100 per bin
drop if t_event < -4 | t_event > 8

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

foreach qt in 0 1 {
	reg rq (ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno)##i.sex if ch_sex==`qt', vce(cluster cidp)

	* store results in a matrix
	matrix M`qt' = (_b[1.t_event#1.sex], _b[1.t_event#1.sex] - invttail(e(df_r),0.025)*_se[1.t_event#1.sex] , _b[1.t_event#1.sex] + invttail(e(df_r),0.025)*_se[1.t_event#1.sex] )	
	qui sum t_event	
	forval xx=2/`r(max)' {
		matrix M`qt' = (M`qt' \ _b[`xx'.t_event#1.sex], _b[`xx'.t_event#1.sex] - invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] , _b[`xx'.t_event#1.sex] + invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] )
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
	   ytitle("Differential impact on mothers relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))    ///
	   legend(order(1 "Boy" 3 "Girl") r(1) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/chsex_2c.png", replace




***************************************************************************
*	GROUPS: Interaction between norm and behavior
*
* 		0. Describe percentages in each interaction group
* 		1. Event study using gender norms as an outcome 
*			1.1. Pooled
*			1.2. Gender differences 
*			1.3. Dividing into gender norm categories
*			1.4. Dividing into gender behavior categories 
*		2. Event study on RQ dividing into gnorm categories
*			2.1. Pooled
*			2.2. Category differences 
*			2.3. Gender differences 

***************************************************************************

*============================================================================
* 	0. Describe percentages in each interaction group
*============================================================================
use newparent_sample.dta, clear

drop if gnorm_q==.
qui levelsof gnorm_q, local(norm)
foreach gn of local norm {
	gen aux`gn' = gnorm_q==`gn'
}
	
label define gb_cat 1 "Traditional" 2 "Egalitarian"
label values gbeh_q gb_cat
graph bar aux1 aux2  , over(gbeh_q) stack percent blabel(bar, position(center) format(%9.02g)) ///
	  bar(1, color(${Grape})) bar(2, color(${Apple})) bar(3, color(${Tangerine})) ///
	  legend(order(2 "Progressive" 1 "Conservative" ) r(5) pos(3) size(small) title("Gender norms:", size(medsmall))) b1title("Domestic labor division")
graph export "${graphs}/norm_beh.png", replace


*============================================================================
* 	1. Event Study using different outcomes
*============================================================================

use newparent_sample.dta, clear

tab t_event ggroup if gnorm!=.
tab t_event ggroup if gbeh!=. 
tab t_event ggroup if rq!=. 

* keep a minimum bin size 
drop if t_event<-3 | t_event>5

* recode factor variable
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* regress different variables 
qui levelsof ggroup, local(quintile) 
foreach var in rq gbeh gnorm {
	foreach qt of local quintile {
		* regress 
		reg `var' ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if ggroup==`qt', vce(cluster cidp)
		
		* save results
		tempfile event_`var'`qt'
		parmest, saving(`event_`var'`qt'', replace)
		
		* transform results file 
		preserve 
		use `event_`var'`qt'', clear
		
		egen event = seq() if strpos(parm, "t_event") > 0
		drop if event == .
		replace event = event - `min'
		
		gen qtile = `qt'
		gen var = "`var'"
		
		save `event_`var'`qt'', replace
		restore
	}
}


use `event_rq1', clear
append using `event_rq2'
append using `event_rq3'
append using `event_rq4'
append using `event_gbeh1'
append using `event_gbeh2'
append using `event_gbeh3'
append using `event_gbeh4'
append using `event_gnorm1'
append using `event_gnorm2'
append using `event_gnorm3'
append using `event_gnorm4'

rename min95 ci_lb
rename max95 ci_ub
gen coeff = round(estimate, 0.001)

*-----------------------------------------------------------------------
*	1.1. Gender norms
*-----------------------------------------------------------------------
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="gnorm", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="gnorm",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==2 & var=="gnorm", lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2 & var=="gnorm",  sort lcolor(`Tangerine')) ///
	   (connected coeff event if qtile==3 & var=="gnorm", lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==3 & var=="gnorm",  sort lcolor(`Cherry')) ///
	   (connected coeff event if qtile==4 & var=="gnorm", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="gbeh",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Gender norms relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Conservative + Traditional" 3 "Conservative + Egalitarian" 5 "Progressive + Traditional" 7 "Progressive + Egalitarian") row(2) pos(north) size(small)) 
graph export "${graphs}/ggroup_1a.png", replace


*-----------------------------------------------------------------------
*	1.2. Domestic labor division 
*-----------------------------------------------------------------------
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="gbeh", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="gbeh",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==2 & var=="gbeh", lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2 & var=="gbeh",  sort lcolor(`Tangerine')) ///
	   (connected coeff event if qtile==3 & var=="gbeh", lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==3 & var=="gbeh",  sort lcolor(`Cherry')) ///
	   (connected coeff event if qtile==4 & var=="gbeh", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="gbeh",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Domestic labor division relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Conservative + Traditional" 3 "Conservative + Egalitarian" 5 "Progressive + Traditional" 7 "Progressive + Egalitarian") row(2) pos(north) size(small)) 
graph export "${graphs}/ggroup_1b.png", replace


*-----------------------------------------------------------------------
*	1.3. RQ
*-----------------------------------------------------------------------
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="rq", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="rq",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==2 & var=="rq", lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2 & var=="rq",  sort lcolor(`Tangerine')) ///
	   (connected coeff event if qtile==3 & var=="rq", lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==3 & var=="rq",  sort lcolor(`Cherry')) ///
	   (connected coeff event if qtile==4 & var=="rq", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="rq",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Conservative + Traditional" 3 "Conservative + Egalitarian" 5 "Progressive + Traditional" 7 "Progressive + Egalitarian") row(2) pos(north) size(small)) 
graph export "${graphs}/ggroup_1c.png", replace

	   
** GB differences, keeping GN constant ** 
* conservatives
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="rq", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="rq",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==2 & var=="rq", lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2 & var=="rq",  sort lcolor(`Tangerine')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Traditional + Conservative" 3 "Egalitarian + Conservative") row(1) pos(12) size(small)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/ggroup_1c_a.png", replace

* progressives
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==3 & var=="rq", lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==3 & var=="rq",  sort lcolor(`Cherry')) ///
	   (connected coeff event if qtile==4 & var=="rq", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="rq",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Traditional + Progressive" 3 "Egalitarian + Progressive") row(1) pos(12) size(small)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/ggroup_1c_b.png", replace
	   
** GB differences, keeping GN constant ** 
* traditionals
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="rq", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="rq",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==3 & var=="rq", lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==3 & var=="rq",  sort lcolor(`Cherry')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Traditional + Conservative" 3 "Traditional + Progressive") row(1) pos(12) size(small)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/ggroup_1c_c.png", replace

* egalitarians 
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==2 & var=="rq", lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==2 & var=="rq",  sort lcolor(`Tangerine')) ///
	   (connected coeff event if qtile==4 & var=="rq", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="rq",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Egalitarian + Conservative" 3 "Egalitarian + Progressive") row(1) pos(12) size(small)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/ggroup_1c_d.png", replace
  
* polar opposites  
sum event
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
twoway (connected coeff event if qtile==1 & var=="rq", lcolor(`Blueberry') lpattern(dash_dot) mcolor(`Blueberry') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==1 & var=="rq",  sort lcolor(`Blueberry')) ///
	   (connected coeff event if qtile==4 & var=="rq", lcolor(`Grape') lpattern(dash_dot) mcolor(`Grape') msymbol("sh")) ///
	   (rcap  ci_lb ci_ub event if qtile==4 & var=="rq",  sort lcolor(`Grape')), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("RQ relative to t=-1", size(medsmall)) ylabel(-1(.5)1, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Traditional + Conservative" 3 "Egalitarian + Progressive") row(1) pos(north) size(medsmall)) 	   