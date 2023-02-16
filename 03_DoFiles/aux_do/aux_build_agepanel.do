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
		keep pidp sex dvage birthy marstat_dv ppid intdaty_dv nchild_dv month
	}
	if "`wno'"!="a" {
		keep pidp sex dvage birthy marstat_dv  month/*livesp_dv cohab_dv*/ ppid /* sppid employ fnpid mnpid npn_dv*/ intdaty_dv nchild_dv newdad newmum
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
drop intdaty
local list5 "a b c d e f g h i j k l"
local list6 "2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 "

local n : word count `list5'

forvalues i=1/`n'{

local c: word `i' of `list5'
local d: word `i' of `list6'
replace year=`d' if wave=="`c'" & month<13  & year==. | year<0  & month!=. & panel=="ukhls"   // input obs to one sample year or another depending on the year they were interview within a wave and if information about starting year is missing
replace year=`d'+1 if wave=="`c'" & month>=13  & year==. | year<0 & month!=. & panel=="ukhls"

}



*---------------------------------------------------------------------------
* 3 . correct cohort and age variables 
*---------------------------------------------------------------------------

* Birth year inconsistencies
egen temp3 = nvals(birthy), by(pidp)
egen temp5 = max(temp3), by(pidp) // how many birthyears per individual
egen temp4 = max(birthy), by(pidp) // maximum birthy by indiv
replace birthy = temp4 if missing(birthy) & temp5==1 // correct birthy for missing birthyears withouth inconsistencies 
drop temp3 temp4

egen byoft = nvals(year), by(pidp birthy) // how often does a birthy appear per indiv?
egen maxby = max(byoft), by(pidp)
gen maxid = (byoft==maxby)
replace maxid=. if maxid==1 & birthy==.
gen temp3 = (maxid*birthy)
egen temp4 = max(temp3), by(pidp)
	replace temp4=. if temp4==0
replace birthy = temp4 if temp5>1 & maxid==0 // correct birthy with the value that appears most often per individual
drop temp*

egen temp3 = nvals(birthy), by(pidp)
egen temp5 = max(temp3), by(pidp)
gen wrong_by = (temp5>1 & !missing(temp5)) // generate id for those we couldn't correct

drop temp* byoft maxby maxid


* Missing age and cohort
replace age = year - birthy if missing(age) & !missing(year) & !missing(birthy)
replace birthy = year - age if missing(birthy) & !missing(year) & !missing(age)

egen temp3 = nvals(birthy), by(pidp)
egen temp5 = max(temp3), by(pidp)
drop temp3
egen byoft = nvals(year), by(pidp birthy)
egen maxby = max(byoft), by(pidp)
gen maxid = (byoft==maxby)
replace maxid=. if maxid==1 & birthy==.
gen temp3 = (maxid*birthy)
egen temp4 = max(temp3), by(pidp)
	replace temp4=. if temp4==0
replace birthy = temp4 if temp5>1 & maxid==0  // correct birthy with the value that appears most often per individual

drop temp*

egen temp3 = nvals(birthy), by(pidp)
egen temp5 = max(temp3), by(pidp)
replace wrong_by = 1 if temp5>1 & !missing(temp5)

drop temp* byoft maxby maxid


* Age and cohort inconsistencies
gen age2 = year - birthy
gen aux_age = age != age2
replace aux_age = 0 if age == age2+1
replace aux_age = 0 if age == age2-1

replace age = age2 if aux_age==1

drop age2 aux_age



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
