/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-25
******************************************************************

This file ...

* 1. couple identifiers for individuals with observed partners 
* 2. one working dataset per gender 
* 3. merge gender files  
* 4. construct gender behavior
* 5. construct assortative mating
* 6. construct new parent variables

**********************
save couple_panel.dta 
**********************

	
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




use "individual_data.dta", clear
sort ${unit}


*============================================================================
* 	1 - Couple identifiers for individuals with observed partners 
*============================================================================
preserve

tempfile coupleid

keep pidp partner wave status tenure
keep if partner!=.
// for 17% of our coupled observations we don't observe partner 

duplicates drop partner wave, force

egen cidp = group(pidp partner)

collapse (first) cidp , by(pidp partner)
// 46,871 couples

egen long first = rowmin(pidp partner)			
egen long second = rowmax(pidp partner)
egen long total = rowtotal(pidp partner)		// to identify individuals with more than one couple

collapse (first) cidp second, by(first total)

rename (first second) (pidp partner)

***************
save `coupleid'
***************

restore


*============================================================================
* 	2 - One working dataset per gender 
*============================================================================

forval sx=0/1 {
	
	preserve
	
	tempfile sex_`sx'
	
	keep if sex==`sx'
	* 60,674 men
	* 62,797 women
	
	** 1. cidp of couples with available partner

	* merge with couple identifier file 
	merge n:1 pidp partner using `coupleid' , keep(1 3) nogen

	rename (pidp partner cidp) (partner pidp cidp1)
	
	if `sx'==0 {
		merge n:1 pidp partner using `coupleid'
	}
	if `sx'==1 {
		merge n:1 pidp partner using `coupleid', keep(1 3) nogen
		append using `sex_0', keep(cidp)
	}
	rename (pidp partner) (partner pidp)

	replace cidp = cidp1 if cidp==.
	drop cidp1
	label variable cidp "Couple identifier"

	** 2. cidp of couples without partner 

	* enumerate such couples
	gen aux1 = status==1 | status==2

	gen aux2 = aux1
	replace aux2 = 0 if pidp==pidp[_n-1] & aux1==1 & aux1[_n-1]==1

	gsort ${unit}
	egen aux3 = seq(), by(pidp aux2)
	replace aux3 = . if aux2==0

	replace aux1 = aux1*aux3 if aux3!=.
	replace aux1 = aux1[_n-1] if aux1==1 & aux1[_n-1]>0 & pidp==pidp[_n-1]
	replace aux1 = . if partner!=. | aux1==0

	* create cidp 
	egen aux_cidp = group(pidp aux1)

	qui sum cidp 
	replace cidp = `r(max)' + aux_cidp if aux1!=.

	drop aux*


	* 3. cidp for single observations 
	gen aux = cidp==.
	egen aux_cidp = group(pidp aux) if aux==1

	qui sum cidp 
	replace cidp = `r(max)' + aux_cidp if aux==1

	drop aux*


	* 4. keep only heterosexual couples 
	egen aux1 = seq(), by(cidp panel wave)
	drop if aux1 > 1
	drop aux1

	* 5. change variables
	if `sx'==0 {
		drop if _merge==2
		drop _merge
		
		rename * m_=
		rename (m_year m_hidp m_panel m_wave m_cidp) (year hidp panel wave cidp)

	}
	if `sx'==1 {
		drop if pidp==.

		rename * f_=
		rename (f_year f_hidp f_panel f_wave f_cidp) (year hidp panel wave cidp)
	}
	
	order *pidp hidp cidp panel wave year

	***************
	save `sex_`sx''
	***************
	
	restore
}


*============================================================================
* 	3 - Merge gender files  
*============================================================================
use `sex_1', clear
merge 1:1 cidp panel wave using `sex_0', nogen

drop if f_pidp==. & m_pidp==.

order cidp f_pidp m_pidp


*============================================================================
* 	4 - Construct event variables
*============================================================================

*----------------------------------------------------------------------
* Unified event variable : Couple's first child 
*----------------------------------------------------------------------
gen event = f_event 
replace event = m_event if m_event==1
replace event = 0 if event==.

label variable event "First child birth"

*----------------------------------------------------------------------
* Event-time: leads and lags around event setting event to 0 (use wave)
*----------------------------------------------------------------------
gen t_event = 0 if event==1

gsort cidp panel wave
replace t_event = t_event[_n-1] + 1 if t_event==. & cidp==cidp[_n-1]

gsort cidp -panel -wave
replace t_event = t_event[_n-1] - 1 if t_event==. & cidp==cidp[_n-1]

gsort cidp panel wave

label variable t_event "Event-time: periods around event"

*----------------------------------------------------------------------
* New parent couple indicator
*----------------------------------------------------------------------
egen newparent = max(event), by(cidp)
label variable newparent "Becomes parent during sample"

drop f_event m_event f_newparent m_newparent 

*----------------------------------------------------------------------
* Couple separation 
*----------------------------------------------------------------------
gen separate = f_separate 
replace separate = m_separate if m_separate == 1
ereplace separate = max(separate), by(cidp)

label variable separate "Couple splits"

drop f_separate m_separate 

*----------------------------------------------------------------------
* Paternity leave 
*----------------------------------------------------------------------
gen dadlv = m_parentlv  

label variable separate "Father on paternity leave"



*============================================================================
* 	4 - Construct domestic labor division index 
*============================================================================

* 1. agreement indicator 
foreach var in huboss hubuys hufrys huiron humops hudiy hupots husits_dv {
	gen `var'_ag = f_`var'_id == m_`var'_id
	replace `var'_ag = . if f_`var'_id==. | m_`var'_id==.
}
egen gbeh_ag = rowmean(huboss_ag hubuys_ag hufrys_ag huiron_ag humops_ag)
label variable gbeh_ag "Domestic labor division agreement"


* 2. factor analysis on female and male indicators
* (a) maximizing available waves
factor m_huboss_id m_hubuys_id m_hufrys_id m_huiron_id m_humops_id f_huboss_id f_hubuys_id f_hufrys_id f_huiron_id f_humops_id		// explains .5678 variation without pcf 
predict gbeh*
drop gbeh2 gbeh3 gbeh4 gbeh5 
rename gbeh1 gbeh
label variable gbeh "Domestic Labor Division"

* (b) maximizing available items
factor  m_huboss_id m_hubuys_id m_hufrys_id m_huiron_id m_humops_id m_hudiy_id m_hupots_id ///
		f_huboss_id f_hubuys_id f_hufrys_id f_huiron_id f_humops_id	f_hudiy_id f_hupots_id	
		// explains .4187 variation without pcf 
predict gbeh*
drop gbeh2 gbeh3 gbeh4 gbeh5 gbeh6 gbeh7
rename gbeh1 gbeh_b
label variable gbeh_b "Domestic division of labor index, more items"


*============================================================================
* 	5 - Classifications
*============================================================================

*----------------------------------------------------------------------
* 1. domestic labor division : new parents at the couple level 
*----------------------------------------------------------------------
preserve 
tempfile gbeh_class 

* use only new parents 
keep if newparent==1

* keep only pre-event observations
gen pre_event = t_event<0
keep if pre_event==1

* household level mean 
collapse (mean) gbeh, by(cidp)

* classification
xtile gbeh_q = gbeh, nquantiles(5)
recode gbeh_q (2=1) (4=2) (5=2)
replace gbeh_q = . if gbeh_q==3

label variable gbeh_q  "Domestic labor division category"
label define gb 1 "Traditional" 2 "Egalitarian", replace
label values gbeh_q gb

* save 
keep cidp gbeh_q
save `gbeh_class', replace 

restore 

* merge with data 
merge n:1 cidp using `gbeh_class', nogen


*----------------------------------------------------------------------
* 2. gender role attitudes: new parents at the individual level 
*----------------------------------------------------------------------
preserve 
tempfile gnorm_class

* use only new parents
keep if newparent==1

* individual level 
keep cidp panel wave f_pidp m_pidp f_gnorm m_gnorm 
rename f_* =1
rename m_* =0
rename (f_* m_*) *
reshape long pidp gnorm, i(cidp panel wave) j(sex) 

* individual level mean 
collapse (mean) gnorm , by(pidp)

* classification 
xtile gnorm_q = gnorm, nquantiles(5)
recode gnorm_q (2=1) (4=2) (5=2)
replace gnorm_q = . if gnorm_q==3

label variable gnorm_q "Gender norm attitude category"
label define gn 1 "Conservative" 2 "Progressive", replace
label values gnorm_q gn

* save 
keep pidp gnorm_q
save `gnorm_class', replace

restore 

* merge with data 
rename f_pidp pidp 
merge n:1 pidp using `gnorm_class', nogen keep(1 3)
rename (pidp gnorm_q) f_=

rename m_pidp pidp 
merge n:1 pidp using `gnorm_class', nogen keep(1 3) 
rename (pidp gnorm_q) m_=



*============================================================================
* 	5 - Construct assortative mating
*============================================================================

gen assort = 1 if f_tertiary==0 & m_tertiary==0
replace assort = 2 if f_tertiary==1 & m_tertiary==0
replace assort = 3 if f_tertiary==0 & m_tertiary==1
replace assort = 4 if f_tertiary==1 & m_tertiary==1
 
label define am 1 "Low Power" 2 "Low M - High F" 3 "High M - Low F" 4 "High Power", replace 
label values assort am

label variable assort "Assortative Mating"
  

***********************************
save full_couple_panel.dta, replace 
***********************************
