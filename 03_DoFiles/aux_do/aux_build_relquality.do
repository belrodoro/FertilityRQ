/*********************************************************************
	This dofile builds a panel of individuals and characteristics
	Raw data file used: indresp.dta
*********************************************************************/

quietly{

*---------------------------------------------------------------------------
* 0 . define wave-specific set of controls 
*---------------------------------------------------------------------------

* BHPS
foreach wno of global BHPSwaves { 
	local controls_b`wno' = "sex ${gbeh_c}"
	
	foreach sw of global gbeh2_bhps_w {
		if "`wno'"=="`sw'" {
			local controls_b`wno' = "`controls_b`wno'' ${gbeh2_c}"		// add controls if waves coincide 
		}
	}
}

* UKHLS
foreach wno of global UKHLSwaves { 
	local controls_`wno' = "${all_c}"
	
	local other_c = "rq rq2 gnorm gbeh gbeh2 spl"
	foreach vg in `other_c' {								// loop over all other variable groups
		foreach sw of global `vg'_w {
			if "`wno'"=="`sw'" {
				local controls_`wno' = "`controls_`wno'' ${`vg'_c}"		// add controls if waves coincide 
			}
		}
	}
}


*---------------------------------------------------------------------------
* 1 . subtract data from each wave 
*---------------------------------------------------------------------------

* BHPS 
foreach wno of global BHPSwaves { 
	
	tempfile wave_b`wno'
	
	* Open raw data file
	use "bhps/b`wno'_indresp.dta", clear
	rename b`wno'_* *
	
	* Save relevant variables
	keep pidp hidp `controls_b`wno''
	cap drop *_bh
	mvdecode *, mv(-10/-1)  
	gen wave = "`wno'"
		
	save `wave_b`wno''	
}

* UKHLS 
foreach wno of global UKHLSwaves { 
	
	tempfile wave_`wno'
	
	* Open raw data file
	use "ukhls/`wno'_indresp.dta", clear
	rename `wno'_* *
	
	* Save relevant variables
	keep pidp hidp `controls_`wno''
	mvdecode *, mv(-10/-1)  
	gen wave = "`wno'"
	
	save `wave_`wno''	
}


*---------------------------------------------------------------------------
* 2 . append all data
*---------------------------------------------------------------------------

use `wave_ba', clear
foreach wno in b c d e f g h i j k l m n o p q r {
	append using `wave_b`wno''
}
gen panel = "BHPS"

foreach wno in $UKHLSwaves {
	append using `wave_`wno''
}
sort wave
replace panel = "UKHLS" if panel==""

*use `wave_a', clear
*foreach wno in b c d e f g h i j k l {
*	append using `wave_`wno''
*}
*gen panel = "UKHLS"

*---------------------------------------------------------------------------
* 3 . change key variables
*---------------------------------------------------------------------------

* generate standardized variables 
qui sum scdascoh_dv
gen std_cohesion = (scdascoh_dv - `r(mean)')/`r(sd)'
la var std_cohesion "Cohesion Measure, standardized"
qui sum scdassat_dv
gen std_satisfaction = (scdassat_dv - `r(mean)')/`r(sd)'
la var std_satisfaction "Satisfaction Measure, standardized"

* correct sex variable
ereplace sex = max(sex), by(pidp)



**************************************
save "${samp}/relquality.dta", replace
**************************************

} // quietly 



