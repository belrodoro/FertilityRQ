/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file carries out the main analysis
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
* 1. main event study analysis
*=============================================================

use newparent_sample.dta, clear

* set a minimum of 100 per bin
drop if t_event < -7 | t_event > 21	

* controls  
event_study rq t_event 1 "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/rq_01.png", replace

* setting the baseline at the previous period
event_study rq t_event 2 "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/rq_01_base.png", replace

* no controls
event_study rq t_event 1 "" cidp "-1(.5).5"
graph display, ysize(5) xsize(9)
graph export "${graphs}/rq_01_nocon.png", replace


*=============================================================
* 2. gender heterogeneity
*============================================================= 

*-------------------------------------------------------------
* 2a. marginal effects 
*-------------------------------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 18

event_margins rq t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_02a.png", replace


*-------------------------------------------------------------
* 2b. significant differences 
*-------------------------------------------------------------
use newparent_sample.dta, clear
drop if t_event < -4 | t_event > 18

event_difference rq t_event 1 sex "${indiv_c} ${couple_c} ib2.wno" cidp "-1(.5).5"
graph display, ysize(5) xsize(5)
graph export "${graphs}/rq_02b.png", replace


*-------------------------------------------------------------
* 2c. predictive margins
*-------------------------------------------------------------
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
graph export "${graphs}/rq_02c.png", replace

/*****

use main_full_data.dta, clear
sort ${unit}

	* 0. variable name
	local varname : variable label rq

	* 1. recode factor variable
	replace t_event = . if t_event < -7 | t_event > 21	
	qui tab t_event, gen(tx)	

	forval xx = 1 (1) 29 {
		replace tx`xx' = 0 if tx`xx'==.
	}
	
	qui sum t_event
		local min = -`r(min)' + 1
		local base = `min' - 1 
	replace t_event = t_event - `min'
	drop tx`base'
	
	* 2. estimate model 
	reg rq i.tx* ${indiv_c} ${couple_c}, vce(cluster pidp) 

	tempfile event_study
	parmest, saving(`event_study', replace)

	* 3. modify stored estimates 
	use `event_study', clear
	
	drop if t == .

	egen event = seq() if strpos(parm, "`event_var'") > 0
	drop if event > 28

	rename min95 ci_lb
	rename max95 ci_ub
	gen coeff = round(estimate, 0.001)

	* 4. plot 
		
	* x-axis labels
	qui sum event
		local lb = `r(min)'
		local ub = `r(max)'
			
	colorpalette lin fruits, locals
	twoway (rarea  ci_lb ci_ub event ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
		   (connected coeff event, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
		   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("Impact on RQ", size(medsmall)) ylabel(#5, labsize(small)) yscale(outergap(*-3) titlegap(*-3))  ///
		   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) yscale(outergap(*-3))  ///
		   legend(off) 
		   
	restore


