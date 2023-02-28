/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-25
******************************************************************

This file ...

	
****************************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {

	do "C:/Users/`c(username)'/OneDrive/Documentos/GitHub/FertilityRQ/03_DoFiles/00_globals.do"

}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}


cd "${samp}"




*============================================================================
* 	1 - Full individual data
*============================================================================

*----------------------------------------------------------------------
* prepare for merge with couple data 
*----------------------------------------------------------------------
use "full_couple_panel.dta", clear
keep cidp f_pidp m_pidp panel wave event t_event newparent separate gbeh* *_gnorm_q dadlv

* merge women 
tempfile women 
preserve 
keep if f_pidp != .
drop m_*
rename f_* *
save `women'
restore

* merge men
tempfile men 
preserve 
keep if m_pidp != .
drop f_*
rename m_* *
save `men'
restore

*----------------------------------------------------------------------
* merge with individual data file
*----------------------------------------------------------------------
use "individual_data.dta", clear
sort ${unit}

drop event newparent separate 

merge 1:1 pidp panel wave using `women', nogen keep(1 3) 
rename (cidp event t_event newparent separate gbeh* gnorm_q dadlv) f_= 

merge 1:1 pidp panel wave using `men', nogen keep(1 3) 

foreach var of varlist cidp event t_event newparent gbeh* gnorm_q dadlv separate {
	replace `var' = f_`var' if `var'==.
}

drop f_*

// attention: since the event variable is defined at the couple level to account for the 1st child of at least one individual in the couple, there are some individuals that have 2 children

**********************************
save "main_full_data.dta", replace
**********************************

*============================================================================
* 	2 - New parent sample 
*============================================================================

use "main_full_data.dta", clear 

* newparents 
keep if newparent == 1
* heterosexual couples 
drop if cidp==.

sort ${unit}

************************************
save "newparent_sample.dta", replace
************************************


*============================================================================
* 	3 - New parent couples 
*============================================================================

use "full_couple_panel.dta", clear 

keep if newparent == 1

sort cidp panel wave

************************************
save "newparent_couples.dta", replace
************************************


