/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file studies impact on variables that could drive the main effect

List of variables:
	* General Happiness
	* Housework Hours 
	* Domestic Labor Division
	* Domestic Labor Division Agreement 
	* Gender Role Attitudes 
	* Worked Hours 
	* Employment Status
	* By RQ item 

Legend: Graph type numbers
		1. pooled 
		2a. gender marginal effects
 		2b. significant differences 
 		2c. predictive margins 
		3. domestic division of labor 
		4. gender role attitudes 
		5. child sex 
		6. education
		7. assortative mating
	
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
* Impact on general happiness (wb)
*
*		1.Pooled 
*		2a. gender marginal effect 
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

replace scghql = 4 - scghql

event_study scghql t_event "${indiv_c} ${couple_c} ib2.wno" cidp
graph export "${graphs}/wb_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

replace scghql = 4 - scghql

event_margins scghql t_event 2 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-.6(.2).6"
graph export "${graphs}/wb_02a.png", replace


*============================================================= 
* Time on household chores: (hw)
*
*		1. pooled 
*		2a. gender marginal effects
* 		2b. significant differences 
*		3. domestic labor division
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

event_study howlng t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph export "${graphs}/wb_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

event_margins howlng t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-5(5)10"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hw_02a.png", replace

*----------------------------------------
* 2b. gender significant differences
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -7 | t_event > 21

event_difference howlng t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/hw_02b.png", replace

*----------------------------------------
* 3f. domestic labor division predict
*----------------------------------------
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
   ytitle("Predictive Margins", size(medsmall)) ylabel(#5, labsize(small)) ///
   xtitle("Event Time (year)", size(medsmall)) ///
   xlabel(1  "-4" 3  "-2" 5 "0" 7  "2" 9 "4" 11 "6" 13 "8", labsize(small))    ///
   title("", size(tiny)) ///
   legend(order(5 "Traditional Fathers" 6 "Egalitarian Fathers" 7 "Traditional Mothers" 8 "Egalitarian Mothers") row(2) pos(12) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/hw_03f.png", replace


*============================================================= 
* Domestic Labor Division (gbeh)
*
*		1. pooled 
*		2a. gender marginal effects
* 		2b. significant differences 
*		3. domestic labor division
*		4. gender role attitudes 
*		5. child sex
*		6. education
*		7. assortative mating
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_study gbeh t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph export "${graphs}/gbeh_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_02a.png", replace

*----------------------------------------
* 2b. gender significant differences
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_difference gbeh t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_02b.png", replace

*----------------------------------------
* 3. domestic labor division 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "-1.5(.5).5"

* regress pooled 
reg gbeh ib`base'.t_event ${indiv_c} ${couple_c} ib2.wno if gbeh_q!=., vce(cluster cidp)
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
graph export "${graphs}/gbeh_03.png", replace

*----------------------------------------
* 4. gender role attitudes
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_04.png", replace

*----------------------------------------
* 5. child sex 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable gbeh "Domestic Labor Division"
label define cs 0 "Boy" 1 "Girl", replace
label values ch_sex cs

event_margins gbeh t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_05.png", replace

*----------------------------------------
* 5.men. child sex on men
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7
keep if sex==0

label variable gbeh "Domestic Labor Division"
label define cs 0 "Boy" 1 "Girl", replace
label values ch_sex cs

event_margins gbeh t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_05_men.png", replace


*----------------------------------------
* 6. tertiary education 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh t_event 1 tertiary "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_06.png", replace

*----------------------------------------
* 7. assortative mating
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

event_margins gbeh t_event 1 assort_q "ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbeh_07.png", replace


*============================================================= 
* Domestic Labor Division Agreemen (gbagree)
*
*		1. pooled 
*		3. domestic labor division
*		4. gender role attitudes 
*		5. child sex
*		7. assortative mating
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_study gbeh_ag t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph export "${graphs}/gbagree_01.png", replace

*----------------------------------------
* 3. domestic labor division 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh_ag t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_03.png", replace

*----------------------------------------
* 4. gender role attitudes
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh_ag t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_04.png", replace

*----------------------------------------
* 5. child sex 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gbeh_ag t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_05.png", replace

*----------------------------------------
* 7. assortative mating
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

event_margins gbeh_ag t_event 1 assort_q "ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gbagree_07.png", replace



*============================================================= 
* Gender Role Attitudes (gnorm)
*
*		1. pooled 
*		2a. gender marginal effects
* 		2b. significant differences 
*		3. domestic labor division
*		4. gender role attitudes 
*		5. child sex
*		7. assortative mating
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_study gnorm t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-.5(.25).5"
graph export "${graphs}/gnorm_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gnorm t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-.5(.25).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_02a.png", replace

*----------------------------------------
* 2b. gender significant differences
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_difference gnorm t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-.5(.25).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_02b.png", replace

*----------------------------------------
* 3. domestic labor division 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gnorm t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "-.5(.25).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_03.png", replace

*----------------------------------------
* 4. gender role attitudes
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gnorm t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_04.png", replace

*----------------------------------------
* 5. child sex 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gnorm t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_05.png", replace

*----------------------------------------
* 5.men. child sex on men
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7
keep if sex==0

label variable gbeh "Domestic Labor Division"
label define cs 0 "Boy" 1 "Girl", replace
label values ch_sex cs

event_margins gnorm t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_05_men.png", replace


*----------------------------------------
* 6. tertiary education 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

event_margins gnorm t_event 1 tertiary "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_06.png", replace

*----------------------------------------
* 7. assortative mating
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

event_margins gnorm t_event 1 assort_q "ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/gnorm_07.png", replace


*============================================================= 
* Worked Hours (jbhrs)
*
*		1. pooled 
*		2a. gender marginal effects
* 		2b. significant differences 
*		3. domestic labor division
*		4. gender role attitudes 
*		5. child sex
*		7. assortative mating
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_study jbhrs t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph export "${graphs}/jbhrs_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_margins jbhrs t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_02a.png", replace

*----------------------------------------
* 2b. gender significant differences
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_difference jbhrs t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_02b.png", replace

*----------------------------------------
* 3. domestic labor division 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_margins jbhrs t_event 1 gbeh_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_03.png", replace

*----------------------------------------
* 4. gender role attitudes
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_margins jbhrs t_event 1 gnorm_q "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_04.png", replace

*----------------------------------------
* 5. child sex 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_margins jbhrs t_event 1 ch_sex "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_05.png", replace

*----------------------------------------
* 6. tertiary education 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable jbhrs "Hours Worked"

event_margins gnorm t_event 1 tertiary "${indiv_c} ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_06.png", replace

*----------------------------------------
* 7. assortative mating
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

label variable jbhrs "Hours Worked"

event_margins jbhrs t_event 1 assort_q "ib25.age i.sex i.employ c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/jbhrs_07.png", replace

*============================================================= 
* Employment Status (employ)
*
*		1. pooled 
*		2a. gender marginal effects
* 		2b. significant differences 
*		3. domestic labor division
*		4. gender role attitudes 
*		5. child sex
*		7. assortative mating
*============================================================= 

*----------------------------------------
* 1. pooled 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_study employ t_event 1 "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph export "${graphs}/employ_01.png", replace

*----------------------------------------
* 2a. gender marginal effect 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_margins employ t_event 1 sex "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_02a.png", replace

*----------------------------------------
* 2b. gender significant differences
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_difference employ t_event 1 sex "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_02b.png", replace

*----------------------------------------
* 3. domestic labor division 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_margins employ t_event 1 gbeh_q "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_03.png", replace

*----------------------------------------
* 4. gender role attitudes
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_margins employ t_event 1 gnorm_q "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_04.png", replace

*----------------------------------------
* 5. child sex 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_margins employ t_event 1 ch_sex "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_05.png", replace

*----------------------------------------
* 6. tertiary education 
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

label variable employ "Employment Status"

event_margins employ t_event 1 tertiary "ib25.age i.sex i.tertiary c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_06.png", replace

*----------------------------------------
* 7. assortative mating
*----------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 7

gen assort_q = 1 if assort==1
replace assort_q = 2 if assort==4

label define aq 1 "Low-Power" 2 "High-Power"
label values assort_q aq

label variable employ "Employment Status"

event_margins employ t_event 1 assort_q "ib25.age i.sex c.lincome i.urban_dv ${couple_c} ib2.wno" cidp "#5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/employ_07.png", replace



***************************************************************************
* 	EFFECT ON EACH ITEM:
*
*		1.1. Pooled 
*		1.2. Separately by gender 
*		1.3. Gender differences  
*		1.4. By gender behavior
***************************************************************************

*-----------------------------------------------------------------------
* 1.1. Pooled
*-----------------------------------------------------------------------
use newparent_sample.dta, clear

* negative items negative 
foreach var in screlpards screlparrg screlparar screlparir {
	replace `var' = `var' - 1
	label values `var' neg_rq
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
	twoway (connected coeff event, lcolor(`Cherry') lpattern(dash_dot) mcolor(`Cherry') msymbol("sh")) ///
		   (rcap  ci_lb ci_ub event ,  sort lcolor(`Cherry')), ///
		   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("", size(medsmall)) ylabel(-.75(.25).5, labsize(small)) yscale(outergap(*-3)) ///
		   xtitle("t", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) xscale(outergap(*-3)) ///
		   graphregion(fcolor(white)) ///
		   legend(off) ///
		   title("`labnum'", size(medlarge)) name(`var', replace) nodraw


}
}

graph combine ${items_time}, c(3) imargin(small)
graph export "${graphs}/fertility_4a_time.png", replace

graph combine ${items_subj}, c(3) imargin(small)
graph export "${graphs}/fertility_4a_subj.png", replace

restore

replace t_event = t_event - `min'


*-----------------------------------------------------------------------
* 1.2. Separately by gender 
*-----------------------------------------------------------------------

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 18

* recode factor variable		--> 	event is t_event==5
sum t_event
	local min = -`r(min)' + 1
	local base = `min' - 1
replace t_event = t_event + `min'

* label variables 
local x = 0
foreach var in $items_subj $items_time {
	
	local x = `x' + 1
	
	* regress
	if `x'<6 {
		qui reg `var' ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${np_couple_c} $items_time ib2.wno, vce(cluster cidp)
	}
	else {
		qui reg `var' ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${np_couple_c} $items_subj ib2.wno, vce(cluster cidp)
	}

	* marginal effects 
	qui margins, dydx(t_event) at(sex=(0 1)) 

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

	matrix M_`var' = (e_male , e_female )
	
	svmat M_`var'

}
	
egen aux = seq() if M_screlpards1!=.
replace aux = aux - `min'

* label variables 
local lablist  `" "consider splitting" "regret getting married" "quarrel" "get on each other's nerves" "degree of happiness" "work together on a project" "stimulating exchange of ideas" "calmly discuss something" "engage in outside interests" "kiss" "'

* plot
local x = 0
foreach var in $items {
	
	local x = `x'+1
	local labnum : word `x' of `lablist' 

	qui sum aux
	local lb = `r(min)'
	local ub = `r(max)'

	colorpalette lin fruits, global opacity(50)
	twoway (connected M_`var'1 aux, lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
		   (rcap  M_`var'2 M_`var'3 aux ,  sort lcolor(${Blueberry})) ///
		   (connected M_`var'4  aux   , sort lcolor(${Cherry}) msymbol("s") lpattern(dash)  mcolor(${Cherry})) ///
		   (rcap      M_`var'5 M_`var'6 aux ,  sort lcolor(${Cherry})), /// 
		   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("", size(tiny)) ylabel(-1(.5)1, labsize(small)) yline(0, lpattern(solid) lcolor(gs11)) ///
		   xtitle("t", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) ///
		   graphregion(fcolor(white)) ///
		   legend(order(1 "Fathers" 3 "Mothers" ) row(1) pos(12) size(medsmall)) ///
	 	   title("`labnum'", size(medlarge)) name(`var', replace) nodraw
	
}

graph combine ${items_time}, c(3) imargin(small)
graph export "${graphs}/fertility_4b_time.png", replace

graph combine ${items_subj}, c(3) imargin(small)
graph export "${graphs}/fertility_4b_subj.png", replace

drop M_*


*-----------------------------------------------------------------------
* 1.3. Gender differences 
*-----------------------------------------------------------------------
* label variables 
local x = 0
foreach var in $items_subj $items_time {
	
	local x = `x' + 1
	
	if `x'<6 {
		qui reg `var' (ib`base'.t_event ${indiv_c} ${np_couple_c} $items_time ib2.wno)##i.sex, vce(cluster cidp)
	}
	else {
		qui reg `var' (ib`base'.t_event ${indiv_c} ${np_couple_c} $items_subj ib2.wno)##i.sex, vce(cluster cidp)
	}

	* save regression results in a matrix
	matrix M_`var' = (_b[1.t_event#1.sex], _b[1.t_event#1.sex] - invttail(e(df_r),0.025)*_se[1.t_event#1.sex] , ///
										   _b[1.t_event#1.sex] + invttail(e(df_r),0.025)*_se[1.t_event#1.sex] )	
	qui sum t_event	
	forval xx=2/`r(max)' {
		matrix M_`var' = (M_`var' \ _b[`xx'.t_event#1.sex], _b[`xx'.t_event#1.sex] - invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] , ///
						  _b[`xx'.t_event#1.sex] + invttail(e(df_r),0.025)*_se[`xx'.t_event#1.sex] )		
	}
	
	svmat M_`var'

}
	

* plot
sum aux
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, locals
local lablist  `" "consider splitting" "regret getting married" "quarrel" "get on each other's nerves" "degree of happiness" "work together on a project" "stimulating exchange of ideas" "calmly discuss something" "engage in outside interests" "kiss" "'
local x = 0
foreach var in $items_subj $items_time {

	local x = `x'+1
	local labnum : word `x' of `lablist' 
	twoway (connected M_`var'1  aux   , sort lcolor(`Blueberry') msymbol("sh") lpattern(dash)  mcolor(`Blueberry')) ///
	   (rcap      M_`var'2 M_`var'3 aux ,  sort lcolor(`Blueberry')), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("", size(medsmall)) ylabel(-.5(.25).5, labsize(small)) yscale(outergap(*-3)) ///
	   xtitle("t", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) xscale(outergap(*-3)) ///
	   legend(off) ///
	   title("`labnum'", size(medlarge)) name(`var', replace) nodraw

}


graph combine ${items_time}, c(3) imargin(small)
graph export "${graphs}/fertility_4c_time.png", replace

graph combine ${items_subj}, c(3) imargin(small)
graph export "${graphs}/fertility_4c_subj.png", replace




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
