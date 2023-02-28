/*********************************************************************
	This dofile builds a panel of ages of all individuals
	Raw data file used: indall.dta
*********************************************************************/

quietly {


*---------------------------------------------------------------------------
* 1 . input age and cohort variables
*---------------------------------------------------------------------------

** British Household Panel Survey **
tempfile agefileBHPS

foreach wno of global BHPSwaves {

	tempfile wave_`wno'
	
	* Open raw data file
	use "bhps/b`wno'_indall.dta", clear
	renpfix b`wno'_ 
	
	* Save relevant variables 
	keep hidp pidp birthy age mastat nchild_dv /*hgest sppid_bh fnpid_bh mnpid_bh*/
	mvdecode *, mv(-10/-1)  
	gen wave="`wno'"

	save `wave_`wno'', replace 
}

use `wave_a', clear
foreach wno in b c d e f g h i j k l m n o p q r {
	append using `wave_`wno''
}

gen panel = "BHPS"

save `agefileBHPS', replace


** Undrestanding Society **
tempfile agefileUKHLS

foreach wno of global UKHLSwaves {

	tempfile wave_`wno'
	
	* Open raw data file
	use "ukhls/`wno'_indall.dta", clear
	renpfix `wno'_ 
	
	* Save relevant variables 
	if "`wno'"=="a" {
		keep pidp sex dvage birthy marstat_dv ppid intdat* nchild_dv
	}
	if "`wno'"!="a" {
		keep pidp sex dvage birthy marstat_dv /*livesp_dv cohab_dv*/ ppid /* sppid employ fnpid mnpid npn_dv*/ intdat* nchild_dv newdad newmum
	}
	rename (dvage marstat_dv intdaty_dv) (age mastat intdaty)
	mvdecode *, mv(-10/-1)  
	gen wave="`wno'"

	save `wave_`wno'', replace 

}

use `wave_a', clear
foreach wno in b c d e f g h i j k l {
	append using `wave_`wno''
}

gen panel = "UKHLS"

save `agefileUKHLS', replace


** Append all data **
append using `agefileBHPS'
sort ${unit}



*---------------------------------------------------------------------------
* 2 . derive period variable
*---------------------------------------------------------------------------
gen year = .

* British Household Panel Survey
local wvno 1991
foreach wv of global BHPSwaves {
	replace year = `wvno' if wave=="`wv'" & panel=="BHPS"
	local wvno `wvno'+1
}

* Understanding Society
replace year = intdaty if panel=="UKHLS"
local wvno 2009
foreach wv of global UKHLSwaves {
	replace year=`wvno' if missing(year) & wave=="`wv'" & panel=="UKHLS"
	local wvno `wvno'+1
}


*---------------------------------------------------------------------------
* 3 . correct cohort and age variables 
*---------------------------------------------------------------------------

*-------------------------------------------
* correct birthyear
*-------------------------------------------
replace birthy = . if birthy>2022

egen birthy_nv = nvals(birthy), by(pidp)		// number of birthyears per person
ereplace birthy_nv = max(birthy_nv), by(pidp)

* use the one most often 
bys pidp birthy: gen oft = _N		            // frequency of each birth year
egen oft_nv = nvals(oft), by(pidp)

egen oft_max = max(oft), by(pidp)               // most frequent

gen birthy_oft = birthy if oft==oft_max & birthy_nv>1 & oft_nv>1
ereplace birthy_oft = max(birthy_oft), by(pidp)
replace oft_nv = 1 if birthy_oft==. // not count if most often is missing

replace birthy = birthy_oft if birthy_oft!=. & birthy!=. & birthy_nv>1 & oft_nv>1
replace birthy_nv = 1 if birthy_nv>1 & oft_nv>1 

drop oft oft_nv oft_max birthy_oft

* else, whichever coincides with age 
gen dvage = year - birthy

gen flag = age!=dvage & year!=. & birthy!=. & age!=.

bys pidp flag: gen oft = _N		        
egen oft_nv = nvals(oft), by(pidp)

egen oft_max = max(oft), by(pidp)               // most frequent

gen birthy_oft = birthy if oft==oft_max & birthy!=. &  birthy_nv>1 & oft_nv>1
ereplace birthy_oft = max(birthy_oft), by(pidp)

replace birthy = birthy_oft if birthy_oft!=. & birthy_nv>1 & oft_nv>1
replace birthy_nv = 1 if birthy_nv>1 & oft_nv>1

drop oft oft_nv oft_max birthy_oft flag dvage

* else, whichever comes first
sort ${unit}
egen first = seq() if birthy!=., by(pidp)
replace first = . if first!=1

gen birthy_oft = birthy if birthy!=. & first==1 & birthy_nv>1
ereplace birthy_oft = max(birthy_oft), by(pidp)

replace birthy = birthy_oft if birthy_oft!=. & birthy_nv>1

drop first birthy_nv birthy_oft

* fill the missing when birthy is ever available 
ereplace birthy = max(birthy), by(pidp)

* individual with no birthy ever 

* fill with year and age 
replace birthy = year-age if birthy==.

egen birthy_nv = nvals(birthy), by(pidp)		// number of birthyears per person
ereplace birthy_nv = max(birthy_nv), by(pidp)

* use the one most often 
bys pidp birthy: gen oft = _N		            // frequency of each birth year
egen oft_nv = nvals(oft), by(pidp)

egen oft_max = max(oft), by(pidp)               // most frequent

gen birthy_oft = birthy if oft==oft_max & birthy_nv>1 & oft_nv>1
ereplace birthy_oft = max(birthy_oft), by(pidp)
replace oft_nv = 1 if birthy_oft==. // not count if most often is missing

replace birthy = birthy_oft if birthy_oft!=. & birthy!=. & birthy_nv>1 & oft_nv>1
replace birthy_nv = 1 if birthy_nv>1 & oft_nv>1 

drop oft oft_nv oft_max birthy_oft

* else, whichever comes first
sort ${unit}
egen first = seq() if birthy!=., by(pidp)
replace first = . if first!=1

gen birthy_oft = birthy if birthy!=. & first==1 & birthy_nv>1
ereplace birthy_oft = max(birthy_oft), by(pidp)

replace birthy = birthy_oft if birthy_oft!=. & birthy_nv>1

drop first birthy_nv birthy_oft

* whenever not repeated
gen flag = birthy==.
ereplace flag = max(flag), by(pidp)

ereplace birthy = max(birthy), by(pidp)

drop flag


*-------------------------------------------
* correct age 
*-------------------------------------------
gen dvage = year - birthy

gen flag = age!=dvage
replace flag = 0 if age==dvage-1  // allow for not having had bday that year yet

replace age = dvage if dvage!=. & dvage>=0

drop dvage flag



* Correct mastat
replace mastat = 1 if mastat == 7
replace mastat = 3 if mastat == 8
replace mastat = 4 if mastat == 9
replace mastat = 5 if mastat == 10

*tab age if mastat==0 & age >16
*gen temp = (mastat==0 & age >16)
*egen temp2 = max(temp), by(pidp)
*br if temp2==1

**** TO DO: - correct mastat =0 when indiv is older than 16 
****		- correct mastat going from married to never married
gen temp = (mastat==0 & age >16)
sort pidp panel wave
replace temp = 1 if mastat==6 & mastat[_n-1]==1 & pidp==pidp[_n-1]
egen wrong_ms = max(temp), by(pidp)
drop temp



************************************
save "${samp}/apc_file.dta", replace
************************************


} // quietly
