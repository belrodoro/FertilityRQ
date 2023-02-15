clear all 
set more off 


if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"



*======================================================================
*	CREATE A COUPLE PANEL 
*======================================================================

use main_full_data.dta, clear  
sort ${unit}

*keep if panel=="UKHLS"

*--------------------------------------------------------------------
* 1. couple identifiers for individuals with observed partners 
*--------------------------------------------------------------------
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


*--------------------------------------------------------------------
* 2. one working dataset per gender 
*--------------------------------------------------------------------

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
	duplicates drop cidp panel wave, force

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




*--------------------------------------------------------------------
* 3. merge gender files  
*--------------------------------------------------------------------
use `sex_1', clear
merge 1:1 cidp panel wave using `sex_0', nogen

order cidp f_pidp m_pidp


*--------------------------------------------------------------------
* 4. construct gender behavior
*--------------------------------------------------------------------

* 1. agreement indicator 
foreach var in huboss hubuys hufrys huiron humops hudiy hupots husits_dv {
	gen `var'_ag = f_`var'_id == m_`var'_id
	replace `var'_ag = . if f_`var'_id==. | m_`var'_id==.
}
egen gbeh_ag = rowmean(huboss_ag hubuys_ag hufrys_ag huiron_ag humops_ag)
label variable gbeh_ag "Share of items agreed upon"


* 2. factor analysis on female and male indicators
* (a) maximizing available waves
factor m_huboss_id m_hubuys_id m_hufrys_id m_huiron_id m_humops_id f_huboss_id f_hubuys_id f_hufrys_id f_huiron_id f_humops_id		// explains .5678 variation without pcf 
predict gbeh*
drop gbeh2 gbeh3 gbeh4 gbeh5 
rename gbeh1 gbeh
label variable gbeh "Domestic division of labor index"

* (b) maximizing available items
factor  m_huboss_id m_hubuys_id m_hufrys_id m_huiron_id m_humops_id m_hudiy_id m_hupots_id ///
		f_huboss_id f_hubuys_id f_hufrys_id f_huiron_id f_humops_id	f_hudiy_id f_hupots_id	
		// explains .4187 variation without pcf 
predict gbeh*
drop gbeh2 gbeh3 gbeh4 gbeh5 gbeh6 gbeh7
rename gbeh1 gbeh_b
label variable gbeh_b "Domestic division of labor index, more items"


*--------------------------------------------------------------------
* 5. construct assortative mating
*--------------------------------------------------------------------
gen assort = 1 if f_tertiary==0 & m_tertiary==0
replace assort = 2 if f_tertiary==1 & m_tertiary==0
replace assort = 3 if f_tertiary==0 & m_tertiary==1
replace assort = 4 if f_tertiary==1 & m_tertiary==1
 

*--------------------------------------------------------------------
* 6. construct new parent variables
*--------------------------------------------------------------------
gen newparent = f_newparent
replace newparent = m_newparent if newparent!=1 & m_newparent!=.
replace newparent = 1 if (f_newmum==1 & f_nchild<2) | (m_newdad==1 & m_nchild<2)

gen dadlv = newparent==1 & m_parentlv==1
ereplace dadlv = max(dadlv), by(cidp)
 
 

******************************
save couple_panel.dta, replace 
******************************

use couple_panel.dta, clear 
