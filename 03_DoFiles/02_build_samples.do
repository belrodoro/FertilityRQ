/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file builds the sample used in the project

	Input:
	main_full_data.dta > BHPS+UKHLS sample

	Output:	
	newparent_sample.dta > sample of individuals becoming parents 91-21

===========================================================
	0. classification on gender norms and behaviors 
===========================================================
	1. newparent couples 
===========================================================
	2. newparent individuals 	
===========================================================

Resulting sample: 8,600 individuals for up to 6 waves 

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


*============================================================================
*	0. classification on gender norms and behaviors 
*============================================================================

*----------------------------------------------------------------
* Domestic labor division categories 
*----------------------------------------------------------------
tempfile gbeh_class

use couple_panel.dta, clear 

* use only new parents 
keep if newparent==1

* keep only pre-event observations
gen pre_event = f_t_event<0
replace pre_event = 1 if m_t_event<0 & f_t_event==.
keep if pre_event==1

* household level mean 
collapse (mean) gbeh gbeh_ag, by(cidp)

* classification
xtile gbeh_q = gbeh, nquantiles(5)
recode gbeh_q (2=1) (4=2) (5=2)
replace gbeh_q = . if gbeh_q==3

xtile gbeh_group = gbeh, nquantiles(2)
label variable gbeh_q  "Domestic labor division category"
label define gb 1 "Traditional" 2 "Egalitarian", replace
label values gbeh_q gb
label variable gbeh_ag "Domestic labor division agreement"

* save 
keep cidp gbeh_ag gbeh_q gbeh_group
save `gbeh_class', replace 

*----------------------------------------------------------------
* Gender norm attitude categories 
*----------------------------------------------------------------
tempfile gnorm_class

use main_full_data.dta, clear  

* use only new parents
keep if newparent==1

* individual level mean 
collapse (mean) gnorm , by(pidp)

* classification 
xtile gnorm_q = gnorm, nquantiles(5)
recode gnorm_q (2=1) (4=2) (5=2)
replace gnorm_q = . if gnorm_q==3

xtile gnorm_group = gnorm, nquantiles(2)
label variable gnorm_q "Gender norm attitude category"
label define gn 1 "Conservative" 2 "Progressive", replace
label values gnorm_q gn

* save 
keep pidp gnorm_q gnorm_group
save `gnorm_class', replace



*============================================================================
*							1. NEW-PARENT COUPLES 
*============================================================================

use couple_panel.dta, clear 

** 0. only observations with available RQ 
*gen aux = f_rq!=.
*replace aux = 1 if m_rq!=.
*ereplace aux = max(aux), by(cidp)

* 1. select only new parents 
keep if newparent==1

* 2. gbeh classification 
merge n:1 cidp using `gbeh_class', keep(1 3) nogen

* 3. gnorm classification 
rename f_pidp pidp
merge n:1 pidp using `gnorm_class', keep(1 3) nogen
rename (pidp gnorm_q m_pidp) (f_pidp f_gnorm_q pidp)
merge n:1 pidp using `gnorm_class', keep(1 3) nogen
rename (pidp gnorm_q) m_=

label variable f_gnorm_q "Gender norm attitude category"
label variable m_gnorm_q "Gender norm attitude category"



***********************************
save newparent_couples.dta, replace
***********************************



*============================================================================
*						2. NEW-PARENT INDIVIDUALS 
*============================================================================

use main_full_data.dta, clear  
sort ${unit}

* 1. merge with couple identifiers
drop newparent     // redefine newparent: from the previous sample we will only preserve COUPLES that had a child, so whenever they break up we stop seeing them 

rename pidp f_pidp 
merge 1:n f_pidp panel wave using "newparent_couples.dta", keep(1 3) nogen keepusing(cidp newparent)
rename (f_pidp cidp newparent) (m_pidp f_cidp f_newparent)
merge 1:n m_pidp panel wave using "newparent_couples.dta", keep(1 3) nogen keepusing(cidp newparent)
rename m_pidp pidp

replace cidp = f_cidp if cidp==.
replace newparent = f_newparent if newparent==.
drop f_cidp f_newparent

* 2. keep only heterosexual couples 
drop if cidp==.

* 3. new parents only 
sort ${unit}
keep if newparent==1

* 4. gbeh & assort classification
merge n:1 cidp panel wave using "newparent_couples.dta", keep(1 3) nogen keepusing(gbeh gbeh_q gbeh_ag gbeh_group assort dadlv)

* 5. gnorm classification
merge n:1 pidp using `gnorm_class', keep(1 3) nogen

* 6. group by gnorm and gbeh 
gen ggroup = 1 if gnorm_group==1 & gbeh_group==1
replace ggroup = 2 if gnorm_group==1 & gbeh_group==2
replace ggroup = 3 if gnorm_group==2 & gbeh_group==1
replace ggroup = 4 if gnorm_group==2 & gbeh_group==2

label define ggcat 1 "Conservative + Traditional" 2 "Conservative + Egalitarian" 3 "Progressive + Traditional" 4 "Progressive + Egalitarian"
label values ggroup ggcat 

* 7. label variables
label variable scghql "General Happiness"

label variable gbeh "Domestic Labor Division"

label define sx 0 "Fathers" 1 "Mothers", replace
label values sex sx

label variable howlng "Weekly Housework Hours"


***********************************
save newparent_sample.dta, replace 
***********************************



/*

/* only observations with relevant information 
keep if panel == "UKHLS"
drop if age==. | tenure==.
drop if age-tenure<18

* keep bins with enough observations 
drop if age<20
drop if age>70
drop if tenure>45*/



********************************
********************************
save full_sample.dta, replace 
********************************
********************************
* individual count			--->		31,359  individuals
egen id = seq(), by(pidp)
replace id = . if id!=1
tab id


