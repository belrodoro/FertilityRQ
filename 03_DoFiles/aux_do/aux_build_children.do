

* ============================================================
*	1 -	Build child data 
* ============================================================

quietly{

* subtract data from each wave 
foreach wno of global UKHLSwaves {  			
	tempfile wave_`wno'
	use "ukhls/`wno'_child.dta", clear
	rename `wno'_* *
	keep pidp hidp sex birthy dvage
	mvdecode *, mv(-10/-1)  
	gen wave = "`wno'"
	save `wave_`wno''	

}

* append all data 
use `wave_a', clear
foreach wno in b c d e f g h i j k l {
	append using `wave_`wno''
}
sort wave
gen panel = "UKHLS"

* children with age 
rename dvage age 
drop if age==.
keep if age<16

* keep oldest sibling 
duplicates tag hidp panel wave, generate(samehh)
egen oldest = min(birthy), by(hidp panel wave)
keep if oldest == birthy

drop samehh oldest

egen oldest = max(age), by(hidp panel wave)
keep if oldest == age

* keep single children 
duplicates tag hidp panel wave, generate(samehh)
keep if samehh==0

drop samehh oldest

* rename 
rename (pidp sex age birthy) ch_=


************************************
save "${samp}/children.dta", replace
************************************



} // quietly 

