/*****************************************************************

Project: Determinants of Relationship Quality
Authors: Belén Rodríguez Moro and Olatz Román Blanco

THIS aux FILE BUILDS TWO PANELS OF MARITAL HISTORIES: SPELL & YEAR LEVEL

Main input:
	phistory_long.dta		-->		Understanding Society 1991-2019 Marital and Cohabitation Histories

Description of raw data: 	
	For UKHLS, initial partnership history and current partnership status were collected in Wave 1. After that information was collected on changes since last interview. 
	For new entrants after Wave 1, only partial information about their past partnership history was collected. 
	For the BHPS sample members information about their partnership history until 2008 has been extracted from the single partnership history file created by Pronzato (2009). 
	For BHPS sample members interviewed as part of Understanding Society (from Wave 2 onwards), their prior information was combined with that collected during these interviews.


======================================================================
 		1	-	CONSTRUCT COMPLETE INDIVIDUAL-SPELL PANEL
======================================================================

We use the raw individual-spell panel and make corrections:

	(i)   Impute missing partners and reasons to end spells
	(ii)  Eliminate duplicated spells 
	(iii) Impute missing end and start dates 
	(iv)  Eliminate spells without any useful information (partner, end, start)
	(v)   Correct overlapping spells
	
	ASSUMPTIONS IN THE IMPUTATION PROCESS:

		ASS1. If spell ended but status is still married, assume they are single (available to mate, but waiting for divorce paperwork)
		ASS2. Transition and last observation: assume individuals remain in their current state 
		ASS3. Last observation of TSM whose spell has ended, assume endy is lastinty
		ASS4. One year overlap, assume the mistake in endy

We create useful variables:

	(i)   Number of separations 
	(ii)  Transition: from current spell to next 
	
Main output:		mhistory_indiv_spell.dta

	- Provides information on all marital spell histories ended from 1991 onwards.
	- Each spell corresponds to an individual who is married, in a civil partnership or in a cohabiting relationship.


======================================================================
 		2	-	CONSTRUCT INDIVIDUAL-YEAR PANEL
======================================================================

Main input:			mhistory_indiv_spell.dta 

Main output:		mhistory_indiv_year.dta 

	- Contains information for all years since 1991 until the last interview 
	- Adds tenure variable, accounting for concatenated spells 
	
	ASS. If no spell information for a given year, assume single 

 
Last modified: 25/10/2022

***********************************************************************/

 
	
use "marital_hist/phistory_long.dta", clear 

qui mvdecode *, mv(-10/-1) 
drop startdate startm enddate endm divorcedate divorcem start_if end_if lastintdate lastintm spellnoR

* drop if lacking essential information
drop if starty==. & endy==.		// 1,921 lost


*======================================================================
* 		1	-	CONSTRUCT COMPLETE INDIVIDUAL-SPELL PANEL
*======================================================================

*-----------------------
* Correct the ordering
*-----------------------

gen flag1 = missing(starty)
replace starty = endy if flag1 == 1
gen flag2 = missing(endy)
replace endy = starty if flag2 == 1

sort pidp ongoing starty endy spellno
egen aux0 = seq(), by(pidp)
label variable aux0 "Spell order corrected"

replace starty = . if flag1 == 1
replace endy   = . if flag2 == 1
drop flag*

sort pidp aux0 lastinty 
order pidp lastinty aux0 partner status starty endy ongoing mrgend cohend 





*-----------------------------------------------------------
* (i) Impute missing partner and reasons for ending spells
*-----------------------------------------------------------

* 1. Partners
replace partner = partner[_n-1] if partner ==. & partner[_n-1] == partner[_n+1]                                            // if missing partner in between spells with same partner: 127 real changes made
replace partner = partner[_n-1] if partner ==. & ongoing[_n-1] == 1 & pidp[_n-1] == pidp[_n]                               // if missing partner and previous spell is ongoing: 13 real changes made

replace partner = partner[_n+1] if partner ==. & (ongoing == 1 | (ongoing == 0 & cohend == 2)) & pidp[_n+1] == pidp[_n]    // if missing partner and current spell is ongoing: 219  real changes made


* 2. Reasons for ending spell		--> if existing mrgend there's no cohend and vv
replace mrgend = 0 if mrgend == 4 & status[_n+1]==status[_n] & partner[_n+1] == partner[_n] & pidp[_n+1] == pidp[_n]       // impute ongoing : 47 real changes made



*-----------------------------------
* (ii) Eliminate duplicated spells 
*-----------------------------------

*** Identical observations 

* exactly identical
duplicates drop pidp lastinty partner status starty endy mrgend cohend pid divorcey ///
			    divorce_if ttl_spells ttl_married ttl_civil_partnership ttl_cohabit ///
			    ever_married ever_civil_partnership ever_cohabit hhorig sampst ///
				ever_proxy_ukhls ever_proxy_bhps, force

* identical in the relevant variables 
duplicates tag pidp partner starty endy if partner!=. & endy!=. & starty!=., gen(flag)

drop if flag==1 & ongoing==0 & pidp==pidp[_n+1] & flag[_n+1]==1 & ongoing[_n+1]==1									   // keep ongoing 
drop if flag==1 & cohend==1 & pidp==pidp[_n+1] & status[_n+1]==2 & pidp==pidp[_n-1] & flag[_n-1]==1 & cohend[_n-1]==2  // drop repeated with wrong ending
drop if flag==1 & (cohend==1 | status==3) & ((pidp==pidp[_n+1] & flag[_n+1]==1) | (pidp==pidp[_n-1] & flag[_n-1]==1))  // drop wrong breakups and civil partnerships vs. marriage
drop if flag==1 & pidp==pidp[_n+1] & flag[_n+1]==1 & status[_n+1]==2 												   // drop cohabitations vs. marriage

drop flag


*** Incomplete observations
duplicates tag pidp partner status if partner!=. , gen(flag)
tab flag

drop if (starty==. | endy==.) & flag>0 & partner!=.			// we eliminate 32 observations (16 relationships) for which we can potentially retrieve informaiton

drop flag


*** Cohabitation to marriage in overlapping relationships
duplicates tag pidp partner status if partner!=. , gen(flag)
replace flag = 0 if status<10 & partner!=.

tab flag

* only for those with 2 duplicates, contiguous and overlapping : 255 real changes made 
replace status = 2          if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1 & starty < endy[_n-1] & cohend[_n-1]==2
replace mrgend = cohend 	if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1	& starty < endy[_n-1] & cohend[_n-1]==2	
replace cohend = .      	if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1	& starty < endy[_n-1] & cohend[_n-1]==2	
replace starty = endy[_n-1] if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1	& starty < endy[_n-1] & cohend[_n-1]==2	
 
* keep longest
replace starty = starty[_n-1] if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1 & starty < endy[_n-1] & cohend[_n-1]!=2	& starty > starty[_n-1]
replace endy = endy[_n-1] if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1 & starty < endy[_n-1]  & cohend[_n-1]!=2	& endy < endy[_n-1]	

gen diff = endy - starty 
drop if pidp == pidp[_n+1] & flag==1 & flag[_n+1]==1 & starty[_n+1]> endy  & cohend!=2 &  diff<diff[_n+1]
drop diff

drop flag



*** Reduce to 2 spells by partner 
duplicates tag pidp partner status if partner!=. , gen(flag)

duplicates tag pidp partner if partner!=. , gen(nsp)		// number of spells with partner

replace endy = starty[_n+2] if nsp==2 & pidp == pidp[_n+1] & nsp[_n+1]==2 & ///
							   pidp == pidp[_n+2] & nsp[_n+2]==2
replace endy = starty[_n+3] if nsp==3 & pidp == pidp[_n+1] & nsp[_n+1]==3 & ///
							   pidp == pidp[_n+2] & nsp[_n+2]==3 & ///
							   pidp == pidp[_n+3] & nsp[_n+3]==3 

drop if nsp==2 & pidp == pidp[_n-1] & nsp[_n-1]==2 & ///
				 pidp == pidp[_n+1] & nsp[_n+1]==2
drop if nsp==3 & pidp == pidp[_n-1] & nsp[_n-1]==3 & ///
				 pidp == pidp[_n+1] & nsp[_n+1]==3 

drop flag nsp			 


*** Three repetitions : keep longest 	
duplicates tag pidp partner status if partner!=. , gen(flag)
			 
replace starty = starty[_n-1] if pidp == pidp[_n-1] & flag==2 & flag[_n-1]==2 & starty > starty[_n-1]
replace endy = endy[_n-1] if pidp == pidp[_n-1] & flag==2 & flag[_n-1]==2 & endy < endy[_n-1]	 
 
drop if flag==2 & aux0<aux0[_n+1] & aux0>aux0[_n-1] & pidp == pidp[_n-1] & pidp == pidp[_n+1] 				 
				 
				 
*** Errors in changing state to married after marriage
replace status = 2 if pidp == pidp[_n-1] & flag==1 & flag[_n-1]==1 & starty == endy[_n-1] & cohend[_n-1]==2
			 
drop flag				 



*---------------------------------
* (iii) Build transition variable 
*---------------------------------

/*
* 1 "Cohabitation - Cohabitation" 
* 2 "Cohabitation - Marriage/Civil Union" 
* 3 "Cohabitation - Singlehood" 
* 4 "Cohabitation - Widowhood" 

* 5 "Marriage - Marriage" 
* 6 "Marriage - Singlehood" 
* 7 " Marriage - Widowhood" 

* 8 "Singlehood- Singlehood"                 // This inflows cannot be constructed now: wait to merge with baseline panel
* 9 "Singlehood - Cohabitation" 
* 10 "Singlehood - Marriage"

label:  status
                 range:  [2,10]                       units:  1
         unique values:  3                        missing .:  0/130,076

            tabulation:  Freq.   Numeric  Label
                        76,887         2  marriage
                           483         3  civil partnership
                        52,706        10  cohabitation (living together as
                                          a couple)

label:  cohend
                 range:  [0,2]                        units:  1
         unique values:  3                        missing .:  71,681/130,076

            tabulation:  Freq.   Numeric  Label
                         9,254         0  ongoing
                        17,074         1  break up
                        32,067         2  marriage
						
label:  mrgend
                 range:  [0,4]                        units:  1
         unique values:  5                        missing .:  52,853/130,076

            tabulation:  Freq.   Numeric  Label
                        45,869         0  ongoing
                         4,165         1  separation
                        18,499         2  divorce
                         7,133         3  widowhood
                         1,557         4  unknown

*/

gen transition = . 

* 1. Cohabitation 																										  
replace transition = 1 if status == 10 & ongoing == 1                                               // Cohabitation - Cohabitation        : 8,880 real changes made
replace transition = 1 if status == 10 & cohend == 0 & transition == .                              //                                    : 800 real changes made
replace transition = 2 if status == 10 & ongoing == 0 & cohend == 2                                 // Cohabitation - Marriage/Civil Union: 26,127 real changes made
replace transition = 3 if status == 10 & ongoing == 0 & cohend == 1                                 // Cohabitation - Singlehood          : 16,883 real changes made

* 2. Marriage 
replace transition = 5 if (status == 2 | status == 3) & ongoing == 1                                // Marriage - Marriage                : 43,329 real changes made
replace transition = 5 if (status == 2 | status == 3) & mrgend == 0 & transition == .               //                                    : 3,437 real changes made
replace transition = 5 if (status == 2 | status == 3) & cohend == 2 & transition == . 				//									   : 0 real changes made			

replace transition = 6 if (status == 2 | status == 3) & ongoing == 0 &  (mrgend == 1 | mrgend == 2) // Marriage - Divorce 				   : 17,344 real changes made
replace transition = 7 if (status == 2 | status == 3) & ongoing == 0 &  mrgend == 3                 // Marriage - Widowhood			   : 7,260 real changes made

label define val 1 "Cohabitation - Cohabitation" 2 "Cohabitation - Marriage/Civil Union" ///
				 3 "Cohabitation - Singlehood" 4 "Cohabitation - Widowhood" ///
				 5 "Marriage - Marriage" 6 "Marriage - Divorce" 7 " Marriage - Widowhood" ///
				 8 "Singlehood- Singlehood"  9 "Singlehood - Cohabitation" 10 "Singlehood - Marriage"
lab value transition val

order pidp lastinty aux0 partner status starty endy ongoing mrgend cohend transition


*-----------------------------------------------
* (iv) Impute missing ending and starting dates
*-----------------------------------------------

* endy   missing .:  5,495/126,752
* starty missing .:  5,482/126,752

* Ends before it starts 
gen flag = endy<starty & endy!=. & starty!=.							
replace flag = 2 if flag[_n-1]==1 & pidp==pidp[_n-1]
replace endy = endy[_n+1] if flag==1 & pidp==pidp[_n+1] & partner==partner[_n+1]
replace ongoing = 1 if flag==1 
drop if flag==2
drop flag

* if relationship continues : 121 real changes made
replace endy = starty[_n+1] if endy ==. & pidp[_n+1] == pidp & (transition == 1 | transition == 2 | transition == 5) & partner[_n+1] == partner & partner!=.	 

* if starty is missing but continue with previous partner: : 58 real changes made 
replace starty = endy[_n-1] if starty == . & pidp[_n-1] == pidp & (transition[_n-1] == 1 | transition[_n-1] == 2 | transition[_n-1] == 5) & partner[_n-1] == partner & partner!=. 

* ASS: last observation of TSM whose spell has ended --> impute endy date to last interview date : 633 real changes made
bysort pidp: gen aux = _N
replace endy = lastinty if aux == aux0 & sampst == 3 & ongoing == 0 & endy == .          
drop aux 


* we generate 1 error here coming from inconsistency
gen flag = endy<starty & endy!=. & starty!=.	
drop if flag==1
drop flag

* merge diagonal observations (101 instances)
gen flag = starty == . & pidp[_n-1] == pidp & (transition[_n-1] == 1 | transition[_n-1] == 2 | transition[_n-1] == 5) & partner[_n-1] == partner & partner!= . 

replace starty = starty[_n-1] if flag==1 & pidp[_n-1] == pidp
drop if flag[_n+1] == 1 & pidp[_n+1] == pidp

rename flag no_mrgy
label variable no_mrgy "Missing Cohabitation to Marriage year"





*----------------------------
* (iv) Number of separations
*----------------------------

gen nsep = transition==3 | transition==6
ereplace nsep = sum(nsep), by(pidp)
label variable nsep "Number of separations"

 

*----------------------------------------
* (v) Drop spells w/o useful information 
*----------------------------------------

sort pidp aux0

gen flag = endy==.
replace endy = endy[_n+1]   if flag==1 & pidp==pidp[_n+1] & ongoing==0
replace endy = starty[_n+1] if flag==1 & pidp==pidp[_n+1] & ongoing==0 & partner!=partner[_n+1]	& starty[_n+1]!=.	

drop if endy<1991			// 18,890 observations deleted 

replace endy = . if flag==1

drop flag




*---------------------------------
* (vi) Spells overlapping on endy 
*---------------------------------

*** Cohabit and marry within the first year w/ same partner
gen flag = pidp==pidp[_n+1] & starty==starty[_n+1] & partner==partner[_n+1]		

gen c_to_m = flag[_n-1]==1 & cohend[_n-1]==2
label variable c_to_m "Transition Cohabitation - Marriage within 1st year"

drop if flag==1		// 1,442 observations deleted
drop flag


*** Cohabit and marry on the last year w/ same partner
gen flag = pidp==pidp[_n-1] & endy==endy[_n-1] & partner==partner[_n-1]	
	
gen m_lasty = flag[_n+1]==1 & status[_n+1]<10
label variable m_lasty "Transition Cohabitation - Marriage within last year"
replace ongoing = 1 if m_lasty == 1

drop if flag==1		// 646 observations deleted 
drop flag


*** End several relationships simultaneously
gen flag = pidp==pidp[_n+1] & endy==endy[_n+1] & endy!=.

replace endy = endy - 1 if flag==1 & starty<=endy-1		// 241 real changes made

drop flag


*** Relationship duplicates with missing partner
duplicates tag pidp status starty endy, gen(flag)

drop if flag==1 & partner==.
drop flag


*** Correct remaining overlaps in endy 
gen flag = pidp==pidp[_n+1] & endy==endy[_n+1] & endy!=.
egen flag2 = max(flag), by(pidp)

*  if missing partner and coincides with others, drop : 9 observations deleted
drop if pidp==pidp[_n-1] & partner==. & (starty==. | end==.) & partner[_n-1]!=. & ///
		((starty[_n-1]!=. & endy[_n-1]==endy) | (starty[_n-1]==starty & endy[_n-1]!=.))
		
		
*  roll back once more : 8 real changes made	
replace endy = endy - 1 if flag==1 & starty<=endy-1		

* fill in with data in missing partner observations : 2 observations deleted
gen flag1 = pidp==pidp[_n+1] & partner!=. & partner[_n+1]==. & (starty==. | end==.) & ///
		((starty[_n+1]!=. & endy[_n+1]==endy) | (starty[_n+1]==starty & endy[_n+1]!=.))
replace flag1 = 1 if pidp==pidp[_n-1] & partner!=. & partner[_n-1]==. & (starty==. | end==.) & ///
		((starty[_n-1]!=. & endy[_n-1]==endy) | (starty[_n-1]==starty & endy[_n-1]!=.))		
drop if flag2==1 & flag1==1
drop flag*

*  drop the missing starty if another spell endy in the same year : 2 observations deleted
gen flag = pidp==pidp[_n+1] & endy==endy[_n+1] & endy!=.
egen flag2 = max(flag), by(pidp)

drop if flag==1 & starty==.

* drop if partner missing : 2 observations deleted 
drop if flag==1 & partner==.
		
drop flag*


*** Duplicates that are not contiguous --> change by hand
duplicates tag pidp endy if endy!=., generate(flag)

replace endy = 2015 if pidp==136514091 & spellno==2

drop if pidp == 136626295 &  (spellno==1  | spellno==4)

replace partner = 137020691 if pidp==137020687 & spellno==3
drop if pidp==137020687 & spellno==4

drop if pidp==207162685 & spellno==3
replace endy = 2016 if pidp==207162685 & spellno==1
replace mrgend = 2 if pidp==207162685 & spellno==1
replace ongoing = 0 if pidp==207162685 & spellno==1

drop if pidp==408973087 & spellno==2

drop if pidp==476157767 & spellno==3

drop if pidp==477069647 & spellno==1
replace partner = 477069651 if pidp==477069647 & spellno==3

replace endy = 2013 if pidp==477933925 & spellno==2
replace mrgend = 2 if pidp==477933925 & spellno==2
replace ongoing = 0 if pidp==477933925 & spellno==2

drop if pidp==479480245 & partner==.
replace endy = 2015 if pidp==479480245 & spellno==1

drop if pidp==680017687 & (spellno==7 | spellno==8)
replace starty = 2016 if pidp==680017687 & (spellno==5)

drop if pidp==680363807 & (spellno==3 |spellno==4)

drop if pidp==680949291 & spellno==5
replace endy = 1999 if pidp==680949291 & spellno==2
replace cohend = 2 if pidp==680949291 & spellno==2

drop if pidp==816146215 & spellno==3
replace endy = 2017 if pidp==816146215 & spellno==2
replace endy = 2016 if pidp==816146215 & spellno==1

drop if pidp==884835727 & spellno==2
replace starty = 1993 if pidp==884835727 & spellno==3

replace endy = 1997 if pidp==1156252967 & spellno==1
drop if pidp==1156252967 & spellno==2

replace endy = 2008 if pidp==1156641927 & spellno==1
drop if pidp==1156641927 & spellno==2

drop if pidp==1168464450 & (spellno==3 | spellno==4)
replace endy = 1987 if pidp==1168464450 & spellno==1 
replace endy = 1997 if pidp==1168464450 & spellno==2

drop if pidp==1224578687 & partner==.

replace endy = 2002 if pidp==1292095207 & spellno==2

drop if pidp==1292932287

drop if pidp==1293304247 & (spellno==4 | spellno==5)

replace endy = 2011 if pidp==1360433847 & spellno==1

replace endy = 1984 if pidp==1360705847 & spellno==1

drop flag




*---------------------------------
* (v) Overlapping spells
*---------------------------------
  cap program drop check 

program check 

quietly {
	cap drop check
	
	tempfile long 
	save `long', replace

	sort pidp aux0 

	* Keep only observations with data for endy and starty
	drop if starty==. | endy==.
	egen aux1 = seq(), by(pidp)
	keep pidp partner starty endy status cohend mrgend aux1

	reshape wide partner starty endy status cohend mrgend, i(pidp) j(aux1)

	* Generate variable for inconsistency counter 
	gen check = 0

	forval base = 1/9 {
	forval aux = 1/9 {	
		if `base'!=`aux' {
			replace check = check + 1 if endy`base'>starty`aux' & endy`base'<endy`aux'
		}
	}	
	}

	keep pidp check
}	
	tab check

	* Merge back to long
	merge 1:n pidp using `long', nogen
	sort pidp starty endy 
	
end


*** Correct divorce year
check

* With correct mrgend
gen flag = pidp==pidp[_n+1] & status==2 & (mrgend==1 | mrgend==2 ) & starty[_n+1]<endy & endy!=. & starty[_n+1]!=.

replace endy = starty[_n+1] if flag==1 & starty[_n+1]!=endy[_n+1]  & check>0 & check!=.   // 147 real changes made 
drop if flag[_n-1]==1 & starty==endy 								// drop individuals with interludios musicales : 7 observations deleted 

drop flag 

* With wrong mrgend 
gen flag = pidp==pidp[_n+1] & status==2 & starty[_n+1]<endy & endy!=. & starty[_n+1]!=.  & check>0 & check!=.
replace endy = starty[_n+1] if flag==1 & starty[_n+1]!=endy[_n+1]   // 63 real changes made 
drop flag



*** Coded corrections 
check 

* sandwiched relationships : 7 observations deleted 
drop if pidp==pidp[_n-1] & pidp==pidp[_n+1] & partner==. & check>0 & check!=. & partner[_n-1]==partner[_n+1] & partner[_n+1]!=.

* missing partner but same dates : 24 observations deleted + 25 observations deleted
drop if pidp==pidp[_n+1] & partner==. & starty==starty[_n+1] & check>0 & check!=.
drop if pidp==pidp[_n-1] & partner==. & starty==starty[_n-1] & check>0 & check!=.

* missing partner but within dates : 82 observations deleted
drop if pidp==pidp[_n-1] & partner==. & starty<endy[_n-1] & endy<endy[_n-1] & check>0 & check!=.

* coh to marr and no partner 
gen flag = pidp==pidp[_n+1] & partner==partner[_n+1] & status==10 & status[_n+1]==2 & cohend==2 & starty[_n+1]>starty & starty[_n+1]<endy 

replace endy = starty[_n+1] if flag==1 & check>0 & check!=. & starty[_n+1]!=endy[_n+1]   // 71 real changes made 
drop  flag

* adjust consecutive cohab with different partners
gen flag = pidp==pidp[_n+1] & partner!=partner[_n+1] & partner!=. & partner[_n+1]!=. & starty[_n+1]>starty & starty[_n+1]<endy 
replace endy = starty[_n+1] if flag==1 & check>0 & check!=. & starty[_n+1]!=endy[_n+1]   // 20 real changes made 

replace endy=2014 if pidp==340697687 & spellno==4
replace endy=2015 if pidp==340697687 & spellno==5
replace starty=2015 if pidp==340697687 & spellno==6

replace endy=2015 if pidp==613920329 & spellno==2
replace mrgend=2 if pidp==613920329 & spellno==2
replace ongoing=0 if pidp==613920329 & spellno==2
replace endy=2018 if pidp==613920329 & spellno==3
replace cohend=0 if pidp==613920329 & spellno==3
replace ongoing=1 if pidp==613920329 & spellno==3

drop flag

* correct wrong interludes : 3 obs deleted 
drop if pidp==pidp[_n-1] & pidp==pidp[_n+1] & partner==. & check>0 & check!=. & ///
	partner[_n-1]==partner[_n+1] & ///
	endy[_n-1]==starty[_n+1] & endy[_n-1]!=. & starty<endy[_n-1] & ///
	cohend[_n-1]==2 & status[_n+1]==2
	

* correct marriage year
check 

gen flag = pidp==pidp[_n-1] & check>0 & check!=. & partner==partner[_n-1] & cohend[_n-1]==2 & (status==2 | status==3)
replace endy=starty[_n+1] if flag==1 & starty[_n+1]!=endy[_n+1] // 30 changes 
drop flag 


*** Remove large checks
check 

* change check == 3
drop if pidp==272171371 & partner==.

* change check == 2
replace endy = lastinty if starty>endy & ongoing==1 & check==2		// 6 real changes 
drop if pidp==749001647 & spellno==3
drop if pidp==1224722171 & spellno==2
replace endy = starty[_n+1] if pidp==pidp[_n+1] & check==2 & partner!=partner[_n+1] & endy>starty[_n+1]		// 7 real changes



*** Remaining check == 1
replace starty = endy[_n-1] if pidp==pidp[_n-1] & (status==2 | status==3) & cohend[_n-1]==2 & check==1 & starty<endy[_n-1]		// 10 real change

drop if endy==. | starty==.


*** Correct negatives 
gen dif = endy-starty if check==1
replace endy = lastinty if dif<0 & ongoing==1
drop dif 


*** Correct last corregible mistakes 
check

* last double mistake 
replace endy = 2009 if check==2 & spellno==1

* if there is one year difference between end and beginning ASSUME the mistake in endyear
gen flag = pidp==pidp[_n+1] & endy==starty[_n+1]+1 & check==1
replace endy = endy-1 if flag==1 & start<=endy-1		// 17 real changes made
drop flag


*** Drop the rest : 58 individuals 
check
drop if check == 1
drop check

*** Correct last repeated pidp endy : 18 duplicates
duplicates tag pidp endy, gen(aux)

replace endy = 1991 if pidp==137082571 & spellno==1 

replace endy = 1978 if pidp==204355647 & spellno==2 

drop if pidp==272354967 & spellno==4
replace ongoing = 0 if pidp==272354967 & spellno==1
replace cohend = 1 if pidp==272354967 & spellno==1

replace endy = lastinty if pidp==409440925 & spellno==6
drop if pidp==409440925 & spellno==5
replace cohend = 2 if pidp==409440925 & spellno==4

replace endy = 1991 if pidp==137082571 & spellno==1 

replace starty = 1994 if pidp==748759567 & spellno==2
drop if pidp==748759567 & spellno==1

drop if pidp==952016327

drop if pidp==1021104327 & spellno==3

replace endy = 2014 if pidp==1101885650 & spellno==2

replace m_lasty = 1 if pidp==1293196892 & spellno==2
drop if pidp==1293196892 & spellno==3

drop aux



*-----------------------------------------------------------
* (vi) Correct order and transition variables after changes 
*-----------------------------------------------------------

sort pidp aux0 

*** Ordering variable
ereplace aux0 = seq(), by(pidp)


*** Transition variable 
replace transition = . 

* 1. Cohabitation 																										  
replace transition = 1 if status == 10 & ongoing == 1                                               // Cohabitation - Cohabitation        : 8,736 real changes made
replace transition = 2 if status == 10 & ongoing == 0 & cohend == 2                                 // Cohabitation - Marriage/Civil Union: 14,112 real changes made
replace transition = 3 if status == 10 & ongoing == 0 & cohend == 1                                 // Cohabitation - Singlehood          : 14,140 real changes made

* 2. Marriage 
replace transition = 5 if (status == 2 | status == 3) & ongoing == 1                                // Marriage - Marriage                : 45,133 real changes made
replace transition = 5 if (status == 2 | status == 3) & mrgend == 0 & transition == .               //                                    : 25 real changes made

replace transition = 6 if (status == 2 | status == 3) & ongoing == 0 &  (mrgend == 1 | mrgend == 2 | mrgend == 4) // Marriage - Divorce 				  : 10,466 real changes made
replace transition = 7 if (status == 2 | status == 3) & ongoing == 0 &  mrgend == 3                 // Marriage - Widowhood			   	  : 4,821 real changes made


order pidp lastinty aux0 partner status starty endy ongoing mrgend cohend transition


***************************************************
save "${samp}/mhistory_indiv_spell.dta", replace
***************************************************




*=========================================================================
* 		2	-	CONSTRUCT INDIVIDUAL-YEAR PANEL 
*=========================================================================

use "${samp}/mhistory_indiv_spell.dta", clear 

sort pidp aux0 

* Keep only observations with data for endy and starty
drop if starty==. | endy==.
drop pid spellno divorcey divorce_if hhorig ever_proxy_ukhls ever_proxy_bhps

qui sum lastinty
	local min = `r(min)'
	local max = `r(max)'

* Reshape wide using spell-varying variables 
global widelist partner starty endy status ongoing mrgend cohend transition no_mrgy c_to_m m_lasty

reshape wide ${widelist} , i(pidp) j(aux0)


* Generate variable per year in panel 
foreach var in $widelist {
forval yr = `min'/`max' {
	gen long `var'`yr' = .
	
	forval base = 1/9 {
		replace `var'`yr' = `var'`base' if starty`base'<`yr' & endy`base'>=`yr'		
	}
}
}


* Drop wide spell panel variables 
foreach var in $widelist {
forval base = 1/9 {
	drop `var'`base' 
}
}

* Reshape long 
reshape long ${widelist} , i(pidp) j(year)
sort pidp year 

* Keep only if within observation period
drop if year > lastinty


*-----------------------------------------------------------
* Generate variables 
*-----------------------------------------------------------

drop endy 


*** Tenure 
gen tenure = year - starty

* If next spell is the continuation of current (marry), add to tenure
gen flag = tenure == 1 & transition[_n-1] == 2 & pidp == pidp[_n+1] 

replace flag = 1 if flag[_n-1] == 1 & pidp == pidp[_n-1] & tenure == tenure[_n-1] + 1
replace tenure = . if flag == 1

replace tenure = tenure[_n-1] + 1 if flag == 1

drop flag 


order pidp year tenure status transition


*** Status
* Codify like mastat
recode status (2 = 1) (3 = 1)  (10 = 2)  

* Single individuals:
* divorced : 8,407 real changes made
replace status = 7 if pidp==pidp[_n-1] & transition[_n-1]==6 & transition==. & status==.

* separated : 8,348 (never married) + 2,787 (separated) real changes made 
replace status = 7 if pidp==pidp[_n-1] & transition[_n-1]==3 & transition==. & status==.

* widowed : 4,753 real changes made 
replace status = 7 if pidp==pidp[_n-1] & (transition[_n-1]==4 | transition[_n-1]==7) & transition==. & status==.

* Roll forward : 580,690 real changes made
gsort pidp year
by pidp: replace status = status[_n-1] if status==. & _n > 1
// we have 500K observations with missing data for all relevant variables 

* Fill single : 423,886 real changes made
egen starty1 = min(starty), by(pidp)
replace status = 7 if status==. & year<=starty1 & starty1!=.
drop starty1

* Label values 
lab define val2 1 "Married" 2 "Living in a couple" 7 "Single"
lab val status val2

lab value transition val


*** Transition : refill it to be yearly
/*
* Keep only for last year in spell 
replace transition = . if pidp==pidp[_n+1] & transition==transition[_n+1] & transition[_n+1]!=.

* Fill year before last, cohabitation : 32,367 real changes made
replace transition = 1 if pidp==pidp[_n+1] & (transition[_n+1]==1 | transition[_n+1]==2 | transition[_n+1]==3 | transition[_n+1]==4) & transition==.

* Fill year before last, marriage : 56,221 real changes made
replace transition = 5 if pidp==pidp[_n+1] & (transition[_n+1]==5 | transition[_n+1]==6 | transition[_n+1]==7) & transition==.

* Roll back : 894,912 real changes made
gsort pidp -year
by pidp: replace transition = transition[_n-1] if transition==. & status==status[_n-1] & _n > 1
gsort pidp year

* Roll forward, continuation transtions: 339,044 real changes made
by pidp: replace transition = transition[_n-1] if transition==. & status==status[_n-1] & (transition[_n-1]==1 | transition[_n-1]==5) & _n > 1


* Transition single - cohabit : 20,181 real changes made
replace transition = 9 if pidp==pidp[_n+1] & status==7 & status[_n+1]==2 & transition==.

* Transition single - married : 16,932 real changes made
replace transition = 10 if pidp==pidp[_n+1] & status==7 & status[_n+1]==1 & transition==.

* Fill singlehood : 8,843 real changes made + 519,730 real changes made
replace transition = 8 if pidp==pidp[_n-1] & transition[_n-1]==3 & status==7 & transition==.
replace transition = 8 if pidp==pidp[_n+1] & status==7 & status==status[_n+1] & transition==.

* Roll forward, breakups: 17,823 real changes made
by pidp: replace transition = transition[_n-1] if transition==. & status==status[_n-1] & _n > 1
*/ 

***************************************************
save "${samp}/mhistory_indiv_year.dta", replace
***************************************************

*=========================================================================
* 		3	-	CONSTRUCT WIDE CROSS-SECTION 
*=========================================================================

use "${samp}/mhistory_indiv_spell.dta", clear 

sort pidp aux0 

* Keep only observations with data for endy and starty
drop if starty==. | endy==.
drop pid spellno divorcey divorce_if hhorig ever_proxy_ukhls ever_proxy_bhps
drop lastinty ongoing mrgend cohend ttl_spells ttl_married ttl_civil_partnership ttl_cohabit ever_married ever_civil_partnership ever_cohabit sampst no_mrgy nsep c_to_m m_lasty

* Reshape wide using spell-varying variables 
reshape wide partner starty endy status transition  , i(pidp) j(aux0)

***************************************************
save "${samp}/mhistory_wide.dta", replace
***************************************************

