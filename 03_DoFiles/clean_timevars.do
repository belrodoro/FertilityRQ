// save "${samp}/clean_timevars.dta", replace
cd "${path}"

use "${samp}/clean_timevars.dta", clear

sort ${unit}

drop *_if

*-------------------------------------------
* number of years
*-------------------------------------------
sort ${unit}

egen year_nv = seq(), by(pidp year)
replace year_nv = year_nv - 1					              // indicator for repeated year

egen year_p = max(year_nv), by(pidp)                          // indic for person w repeated year

** Last repeated year **
egen year_num = sum(year_nv), by(pidp)						  // number of repeated years

egen year_rep = seq(), by(pidp year_nv)
replace year_rep = 0 if year_nv==0 | year_rep!=year_num       // only last repeated year
replace year_rep = 1 if year_rep>0

gen year_doub = year_nv*year
ereplace year_doub = max(year_doub), by(pidp)                 // repeated year 

replace year_rep= 1 if year_rep[_n-1]==1 & year==year[_n-1]+1 // indic if adding 1 would induce overlap again

gsort ${unit}
egen year_add = seq(), by(pidp year_rep)                      // aux to add to repeated year
replace year_add = 0 if year_rep==0

gen year_dv = year                                            // corrected year variable

replace year_dv = year + year_add if  year_p==1

drop year_nv year_num year_rep year_p year_doub year_add


egen year_nv = seq(), by(pidp year_dv)
replace year_nv = year_nv - 1					              // indicator for repeated year

egen year_p = max(year_nv), by(pidp)                          // indic for person w repeated year

egen year_num = sum(year_nv), by(pidp)						  // number of repeated years

tab year_num




br pidp year year_dv intdat* year_nv if year_p>0

br pidp year intdat* year_nv if pidp==2853965


/*
|![Graph](/assets/img/1.jpg)|Some text|


										
