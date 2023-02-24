/**********************************************************************
 this do file constructs 2 files:
	- a panel of individual - biological child
	- a cross-section of individuals and their first borns (no twins)
 it uses mainly egoalt.dta, but corrects child birthyears using child.dta 
 
 IMPORTANT: run after building apc files 
**********************************************************************/

tempfile temp_child_panel

*---------------------------------------------------------------------------
* 1 . Understanding Society
*---------------------------------------------------------------------------

tempfile ukhls
foreach wno of global UKHLSwaves {
	
	tempfile `wno'_wave
	
	use "ukhls/`wno'_egoalt.dta", clear
	renpfix `wno'_    
	
	order hidp pidp 
	sort hidp pno pidp apidp
	keep hidp pidp apidp sex asex relationship_dv  
	mvdecode *, mv(-10/-1)

	* keep only children
	keep if relationship_dv == 9 		// natural parent to alter 
	drop relationship_dv
	rename (apidp asex) (ch_pidp ch_sex)

	duplicates drop pidp, force // single obs per individual
	gen wave = "`wno'"

	save ``wno'_wave', replace 
}

* Append all waves

use `a_wave', clear
foreach wno in b c d e f g h i j k l {
	append using ``wno'_wave'
}
gen panel = "UKHLS"
save `ukhls', replace



*---------------------------------------------------------------------------
* 2 . british household panel survey
*---------------------------------------------------------------------------

tempfile bhps
foreach wno of global BHPSwaves {

	tempfile `wno'_wave
		
	use "bhps/b`wno'_egoalt.dta", clear
	renpfix b`wno'_ 

	order hidp pidp 
	sort hidp pno pidp apidp
	keep hidp pidp apidp relationship_bh esex asex
	mvdecode *, mv(-10/-1)     
	rename (relationship_bh esex) (relationship_dv sex)  

	* keep only children
	keep if relationship_dv == 9 		// natural parent to alter 
	drop relationship_dv

	rename (apidp asex) (ch_pidp ch_sex)

	duplicates drop pidp, force // single obs per individual

	gen wave = "`wno'"

	save ``wno'_wave', replace 

}

* Append all waves 
use `a_wave', clear
foreach wno in b c d e f g h i j k l m n o p q r {
	append using ``wno'_wave'
}

gen panel = "BHPS"

save `bhps', replace


*---------------------------------------------------------------------------
* 3 . append all
*---------------------------------------------------------------------------

append using `ukhls'
sort ${unit}

*---------------------------------------------------------------------------
* 4 . create individual - child panel 
*---------------------------------------------------------------------------

* flag sex inconsistencies 
egen flag_chsex = nvals(ch_sex), by(ch_pidp)
replace flag_chsex = flag_chsex - 1

egen flag_sex = nvals(sex), by(pidp)
replace flag_sex = flag_sex - 1

* keep 1 obs per individual - child pair 
drop wave panel hidp

duplicates drop pidp ch_pidp, force
sort pidp ch_pidp

save `temp_child_panel', replace

*---------------------------------------------------------------------------
* 5 . merge with birth year 
*---------------------------------------------------------------------------

use "${samp}/apc_file.dta", clear

keep pidp birthy
drop if birthy==.
duplicates drop pidp, force 

rename * ch_=

* merge with individual child panel 
merge 1:n ch_pidp using `temp_child_panel', keep(2 3) nogen
//     Not matched                            40
//     Matched                            61,225  

* order by birth 
sort pidp ch_birthy
egen ch_num = seq(), by(pidp)

* total children 
egen ch_total = max(ch_num), by(pidp)
label variable ch_total "Total number of biological children"

order pidp sex ch_pidp ch_birthy ch_sex ch_num

notes: individual-child panel
***************************************
save "${samp}/child_panel.dta", replace
***************************************

*---------------------------------------------------------------------------
* 6 . save first child panel 
*---------------------------------------------------------------------------

egen aux = seq(), by(pidp ch_birthy)
ereplace aux = max(aux), by(pidp ch_birthy)  // identify twins, triplets...

keep if ch_num==1 & aux==1                   // single first children 

drop aux ch_num flag_chsex flag_sex


notes: cross-section of individuals and their first born
***************************************
save "${samp}/first_child.dta", replace
***************************************
