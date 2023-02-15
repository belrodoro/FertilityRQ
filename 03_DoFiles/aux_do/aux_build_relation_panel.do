/*********************************************************************
	This dofile builds a panel of individuals and relation to hh members
	Raw data file used: egoalt.dta
*********************************************************************/

quietly {

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
	keep hidp pidp pno apidp apno sex asex relationship_dv  
	mvdecode *, mv(-10/-1)

	* a. Parents' personal identifiers
	gen temp = relationship_dv == 4 // indicator of natural child
	egen parentnum = seq() if temp == 1, by(pidp)
	levelsof parentnum, l(num) 
	foreach j of local num {
		gen long temp`j' = apidp if parentnum == `j'
		egen long parent`j' = max(temp`j'), by (pidp) // a variable per parent
	}
	drop temp* parentnum

	* b. Partner's personal identifier 
	gen long temp = apidp if relationship_dv <= 3   
	egen long partner = max(temp), by(pidp) // a variable per partner
	drop temp

	* c. Number of children
	gen temp = relationship_dv == 9 // natural parent
	egen nchild = sum(temp), by(pidp)	// total number of kids


	* d. Identifier of the children
	gen long child_pidp = apidp if temp == 1
	gen child_sex		= asex if temp == 1

	drop temp 

	duplicates drop pidp, force // single obs per individual
	drop pno apidp apno relationship_dv asex

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
	keep hidp pidp apidp pno apno relationship_bh esex asex
	mvdecode *, mv(-10/-1)     
	rename (relationship_bh esex) (relationship_dv sex)  

	* a. Parents' personal identifiers
	gen temp = relationship_dv == 4 // indicator of natural child
	egen parentnum = seq() if temp == 1, by(pidp)
	levelsof parentnum, l(num) 
	foreach j of local num {
		gen long temp`j' = apidp if parentnum == `j'
		egen long parent`j' = max(temp`j'), by (pidp) // a variable per parent
	}
	drop temp* parentnum

	* b. Partner's personal identifier
	gen long temp = apidp if relationship_dv == 1 | relationship_dv == 2
	egen long partner = max(temp), by(pidp) // a variable per partner
	drop temp

	* c. Number of children
	gen temp = relationship_dv == 9 // natural parent
	egen nchild = sum(temp), by(pidp)	// total number of kids

	* d. Identifier of the children
	gen long child_pidp = apidp if temp == 1

	drop temp 

	duplicates drop pidp, force // single obs per individual
	drop pno apidp apno relationship_dv asex

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
* 4 . generate pidp variables for each parent
*---------------------------------------------------------------------------

gen long np1_pidp = . // id of natural parent #1
gen long np2_pidp = . // id of natural parent #2


egen order = seq(), by(pidp)	// lists observations per individual

egen temp1 = nvals(parent1), by(pidp)
egen n_np1 = max(temp1), by(pidp) // amount of NP1
	replace n_np1 = 0 if missing(n_np1)
egen temp2 = nvals(parent2), by(pidp)
egen n_np2 = max(temp2), by(pidp) // amount of NP2
	replace n_np2 = 0 if missing(n_np2)

gen long temp4 = parent1 if order==1
levelsof order, l(ord)
foreach wno of local ord {
	egen long temp5 = max(temp4), by(pidp)
	replace temp4 = parent1 if order==`wno' & missing(temp5)
	drop temp5
}
egen long p1 = max(temp4), by(pidp) // id of first NP1 of individual

gen long temp6 = parent2 if order==1
levelsof order, l(ord)
foreach wno of local ord {
	egen long temp5 = max(temp6), by(pidp)
	gen check = (p1==temp5 & !missing(p1) & !missing(temp5))
	replace temp6 = . if check==1
	replace temp6 = parent1 if order==`wno' & check==1
	replace temp6 = parent2 if order==`wno' & missing(temp5)
	drop temp5 check
}
egen long p2 = max(temp6), by(pidp) // id of first NP2 of individual

gen temp8 = (parent1!=p1 & parent1!=p2 & !missing(parent1))
egen temp9 = max(temp8), by(pidp) // indicator: parent1 != to p1 or p2
gen temp10 = (parent2!=p1 & parent2!=p2 & !missing(parent2))
egen temp11 = max(temp10), by(pidp) // indicator: parent != to p1 or p2

* Fill in id for natural parent #1
replace np1_pidp=p1 if temp9==0
replace np1_pidp=p1 if temp9==1 & n_np1 <= 2 & missing(p2)
* the remainders have more than 2 different natural parents --> drop

* Fill in id for natural parent #2
replace np2_pidp=p2 if temp11==0

drop temp* p1 p2 order n_np*


*---------------------------------------------------------------------------
* 5 . label variables
*---------------------------------------------------------------------------

la var parent1  "Personal id of cohabiting parent #1"
la var parent2  "Personal id of cohabiting parent #2"
la var partner  "Personal id of cohabiting partner"
la var nchild   "Number of natural children"
la var np1_pidp "PIDP parent #1"
la var np2_pidp "PIDP parent #2"
la var wave     "Wave"
la var panel    "Panel"


note: Panel of parental relations
******************************************
save "${samp}/relation_panel.dta", replace
******************************************



} // quietly

