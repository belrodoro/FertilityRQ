/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-25
******************************************************************

This file cleans and puts together all the raw data from Understanding Society

	Output: raw_data.dta > Full individual level BHPS+UKHLS data
===================================================================
			1 - Build raw and merge data files
===================================================================

Using raw BHPS and UKHLS files, build the following sets of information:

(1) Regressors: RQ and controls

	Input: indresp.dta (waves 1 3 5 7 9 11)
	Output:	relquality.dta > individuals w/ RQ or controls
	Variables of interest:
		- RQ variables: 
				+ separate items: screlparwt screlparei screlparcd scparoutint screlpards screlparrg screlparar screlparir screlparks screlhappy
				+ built measures: scdassat_dv scdascoh_dv
		- Controls:
				+ age: dvage birthy 
				+ sex: sex
				+ race: racel 
				+ education: qfhigh_dv
				+ employment status: employ
				+ type of job: jbterm1 jbsemp
				+ marriage status: mastat_dv
				+ family type: nchild_dv ndepchl_dv hhtype_dv
				+ income: fimngrs_dv fimnnet_dv 
				+ country: England, Wales, Scotland, Northern Ireland		
				+ general happiness: scghql
				+ hours worked: jbhrs
				+ subjective wellbeing: scghq1_dv
				
(2) Time variables 

	Input: indall.dta 
	Output: apc_file.dta > individuals w/ time variables
	Variables of interest: 
		- APC: age year birthy 
		- Marriage status (unified): mastat

(3) Tenure

	Input: phistory_long.dta 
	Output: mhistory_indiv_year.dta > individuals w/ marital history
	
(4) First child 

	Input: family matrix and egoalt files

===================================================================
  3 - Use individual response info to fill marital histories in 
===================================================================

(1) Fill 'status' with individual information on marital status 
(2) Make corrections to tenure and partner 
(3) Convert transition into yearly variable
(4) Document first relationship

===================================================================
			4 - Clean and prepare general controls
===================================================================	

sex, education, wave, controls 

************
raw_data.dta 
************
	
****************************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:/Users/belen/Onedrive/Documentos/GitHub/FertilityRQ/03_DoFiles/00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}


*============================================================================
* 	1 - Build raw data files 
*============================================================================

cd "${path}"

// * (1) Regressor panel: obtain RQ and individual characteristics
 quietly do "${dofiles}/aux_do/aux_build_relquality.do"
//
// * (2) Age-cohort-period panel: fill in all individuals' ages and periods 
 quietly do "${dofiles}/aux_do/aux_build_agepanel.do"
//
// * (3) Marital history panel: build panel of relationships
 quietly do "${dofiles}/aux_do/aux_build_tenure.do"
//
// * (4) Household relation history: identify partners
// quietly do "${dofiles}/aux_do/aux_build_relation_panel.do" 
//
// * (5) Information about first borns
 quietly do "${dofiles}/aux_do/aux_child_panel.do" 


*============================================================================
* 	2 - Merge all raw datasets into an individual-year panel
*============================================================================
cd "${samp}"

use "relquality.dta", clear 														// characteristics
	merge 1:1 ${unit} using "apc_file.dta", nogen keepusing(age year birthy mastat sex nchild_dv newmum newdad)	// time variables
	merge 1:1 ${unit} using "relation_panel.dta", nogen keepusing(partner)			// family matrix
		rename partner partner_fm
	merge n:1 pidp year using "mhistory_indiv_year.dta",  keep(1 3)					// marital history 
	merge n:1 pidp using "first_child.dta", keep(1 3) nogen							// first child data



*============================================================================
*	3 - Clean marital histories 
*============================================================================

order pidp year birthy sex age mastat status transition tenure lastinty
gsort ${unit}


*---------------------------------------------------------------------------
* (1) Fill 'status' with individual information on marital status 
*---------------------------------------------------------------------------
ereplace lastinty = max(lastinty), by(pidp)

* ASS. If mastat = never married & no marital history info, assume single
replace status = 7 if mastat == 6 & lastinty ==. & status == .                                           // real changes: 143,405 

* If mastat = never married & year<= lastinty
replace status = 7 if mastat == 6 & year<= lastinty & status == .                                         // real changes: 2,733 

* If mastat = widow & lastinty = ., single: individual remains widow
replace status = 7 if mastat == 3 & lastinty ==. & status == .                                            // 17,860  real changes

* If mastat = cohabit/marry & partner_fm!=. & lastinty <= year, update status & partner
replace status = mastat if partner_fm !=. & lastinty <= year & status == . & (mastat == 1 | mastat == 2)  // 2,035 real changes made

* If no marital history info for individual, impute mastat
replace status = 1 if mastat == 1 & status == . & _merge == 1 & mastat != . 
replace status = 2 if mastat == 2  & status == . & _merge == 1 & mastat != . 
replace status = 7 if (mastat == 3 | mastat== 4 |mastat == 5 | mastat == 6) & status == . & _merge == 1 & mastat != . 

* If info on partner/RQ is given, mastat != single, but status == single, correct status
replace status = 1 if mastat == 1 & (status == 7 | status == .) & (partner_fm!=. | scdassat_dv!=. | scdascoh_dv !=.)
replace status = 2 if mastat == 2 & (status == 7 | status == .) & (partner_fm!=. | scdassat_dv!=. | scdascoh_dv !=.)

* If lastinty == last observed year, impute mastat 
by pidp: replace status = mastat if  (mastat == 3 | mastat== 4 |mastat == 5 | mastat == 6) & sampst == 3 & year == lastinty & _n == _N

* Single status
replace status = 7 if status==3 | status==4 | status==5 | status==6

* Replace singles using mastat 
replace status = 7 if status==. & mastat!=. & mastat>2

// Still 21% missing in status for UKHLS, from underage


* Label status variable 
label variable status "Marital status"
label define val2 1 "Married" 2 "Living in a couple" 7 "Single", replace
label values status val2

gen single = (status == 7)
label variable single "Single individual"


*---------------------------------------------------------------------------
* (2) Correct tenure and partner 
*---------------------------------------------------------------------------

*** Correct tenure if marriage continues
replace tenure = tenure[_n-1] + 1 if  pidp==pidp[_n-1] & (status == 1 | status ==2) & partner_fm == partner_fm[_n-1] & partner_fm!= . & lastinty < year & tenure == . 
label variable tenure "Relationship tenure"

drop _merge*

*** Fill in missing partner
replace partner = partner[_n-1] if pidp==pidp[_n-1] & tenure-tenure[_n-1]==year-year[_n-1] & partner==. & partner[_n-1]!=. & screlparei!=.				// when we have info on rq
replace partner = partner[_n-1] if pidp==pidp[_n-1] & tenure-tenure[_n-1]==year-year[_n-1] & partner==. & partner[_n-1]!=. & (status==1 | status==2)	// when we have info on mastat

*** Fix duplicated partners 
duplicates tag partner wave if panel=="UKHLS", gen(aux)
replace aux=. if partner==.

qui levelsof partner if aux==1, local(par)
foreach pp of local par {
	qui levelsof wave if partner==`pp' & aux==1, local(wav)
	foreach ww of local wav {
		qui levelsof partner if pidp==`pp' & wave=="`ww'" & panel=="UKHLS", local(true)
		capture replace aux =2 if pidp==`true' & wave=="`ww'" & aux==1
	}
}

replace partner = . if aux==1
// there are 4 partner id-s who have missing partners 
drop aux



*---------------------------------------------------------------------------
* (3) Convert transition into yearly variable
*---------------------------------------------------------------------------
replace transition = . if pidp==pidp[_n+1] & transition==transition[_n+1] & transition[_n+1]!=.


* Transition single - cohabit : 20,181 real changes made
replace transition = 9 if pidp==pidp[_n+1] & status==7 & status[_n+1]==2 

* Transition single - married : 16,932 real changes made
replace transition = 10 if pidp==pidp[_n+1] & status==7 & status[_n+1]==1 & transition==.

* Fill single : 8,843 real changes made + 519,730 real changes made
replace transition = 8 if pidp==pidp[_n-1] & transition[_n-1]==3 & status==7 & status==status[_n+1]  & transition==.
replace transition = 8 if pidp==pidp[_n+1] & status==7 & status==status[_n+1] & transition==.
replace transition = 8 if pidp==pidp[_n+1] & status==7 & ( transition[_n+1] == 9 | transition[_n+1] == 10) & transition==.


* Transition married - married or cohabit - cohabit 
replace transition = 5 if  pidp==pidp[_n+1] & status == 1 & status[_n+1] == 1 & transition == . 
replace transition = 1 if  pidp==pidp[_n+1] & status == 2 & status[_n+1] == 2 & transition == .
 
* Transition cohabit - married
replace transition = 2 if pidp==pidp[_n+1] & status == 2 & status[_n+1] == 1 & transition == . 

* Transition cohabit - single
replace transition = 3 if pidp==pidp[_n+1] & status == 2 & status[_n+1] == 7 & transition == . 

* Transition marriage - single
replace transition = 6 if pidp==pidp[_n+1] & status == 1 & status[_n+1] == 7 & transition == . 


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

* Roll forward, breakups: 17,823 real changes made
by pidp: replace transition = transition[_n-1] if transition==. & status==status[_n-1] & _n > 1 & status!=. 


* Leave last transition as missing
by pidp: replace transition = . if year > lastinty & _n == _N


* Label transition variable 
label variable transition "Relationship transition from current period to next period"
label define val 1 "Cohabitation - Cohabitation" 2 "Cohabitation - Marriage/Civil Union" ///
				 3 "Cohabitation - Singlehood" 4 "Cohabitation - Widowhood" ///
				 5 "Marriage - Marriage" 6 "Marriage - Singlehood" 7 " Marriage - Widowhood" ///
				 8 "Singlehood- Singlehood"  9 "Singlehood - Cohabitation" 10 "Singlehood - Marriage", replace
label value transition val



*---------------------------------------------------------------------------
* (4) Document first relationship
*---------------------------------------------------------------------------

egen starty1 = min(starty), by(pidp)
gen fr_age = starty1 - birthy 	
label variable fr_age "Age of first relationship"

gen tenure1 = tenure if starty==starty1
ereplace tenure1 = max(tenure1), by(pidp)
gen fr_tenure = tenure1 			
label variable fr_tenure "Tenure of first relationship"

drop starty1 tenure1


*============================================================================
* 	4 - Clean and prepare general controls 
*============================================================================

* (a) Sex 
ereplace sex = max(sex), by(pidp)

* (b) Education 
// It follows International Standard Classification of Education (ES-ISCED):  chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/viewer.html?pdfurl=https%3A%2F%2Fdam.ukdataservice.ac.uk%2Fmedia%2F262853%2Fdiscover_sqb_education_schneider.pdf&clen=548821&chunk=true page 16
gen education = . 
replace education = 1 if qfhigh_dv == 12										// no qualification
replace education = 2 if (qfhigh_dv == 9 | qfhigh_dv == 8)						// lower secondary qualification
replace education = 3 if (qfhigh_dv == 6 | qfhigh_dv == 7 | qfhigh_dv == 10)	// upper secondary qualification
replace education = 4 if qfhigh_dv == 11
replace education = 5 if (qfhigh_dv == 5 | qfhigh_dv == 4 | qfhigh_dv == 3)		// Post-secondary below bachelor levels
replace education = 6 if qfhigh_dv == 2											// Bachelor degree
replace education = 7 if qfhigh_dv == 1											// Higher tertiary education

la def educ 1 "No qualification" 2 "Lower Secondary" 3 "Upper Secondary" 4 "" 5 "Post-secondary, below bachelor" 6 "Bachelors Degree" 7 "Higher Tertuary", replace
la val education educ

* (c) Child sex
replace ch_sex = ch_sex - 1 
label define ba_asex 0 "Boy" 1 "Girl", replace 
replace ch_sex = 0 if ch_sex==-1

* (c) Wave indicators 
encode wave, gen(wno)

* (d) Prepare controls
replace age = . if age<0

gen tertiary = education==6 | education==7
la de tert 0 "No College" 1 "College", replace
la val tertiary tert 
la var tertiary "Has a College Degree"

recode employ (2=0)
la de emp 0 "Unemployed" 1 "Employed", replace
la val employ emp 
la var employ "Is Employed"

recode sex (1=0) (2=1)
la de sex 0 "Male" 1 "Female", replace
la val sex sex 
la var sex "Sex"

recode urban_dv (2=0)
la de urb 0 "Rural" 1 "Urban", replace
la val urban_dv urb
la var urban_dv "Lives in an urban area"

replace fimngrs_dv = 0 if fimngrs_dv<0
gen lincome = log(fimngrs_dv)
*la var lincome "Log Monthly Personal Income (Gross)"
la var lincome "Log Personal Income"

gen couple = status==1 | status==2
la de coup 0 "Single" 1 "In couple", replace
la val couple coup
la var couple "Is in a Couple"

gen married = status==1
la de marr 0 "Unmarried" 1 "Married", replace
la val married marr
la var married "Is Married"

gen separate = transition==3 |transition==6
ereplace separate = max(separate), by(pidp partner)
la var separate "Couple ever separates"

la var nchild "Number of children in household"

gen child = nchild>0
label define ch 0 "No children" 1 "At least one child", replace
label values child ch 
la var child "Has at least one child in household"

gen ch1 = nchild>0 & nchild!=.
gen ch2 = nchild>1 & nchild!=.
gen ch3 = nchild>2 & nchild!=.
gen ch4 = nchild>3 & nchild!=.

la def one 1 "One child", replace
la def two 1 "Two children", replace
la def three 1 "Three children", replace
la def four 1 "Four or more children", replace
la val ch1 one 
la val ch2 two 
la val ch3 three
la val ch4 four

label variable scghql "General Happiness"

label define sx 0 "Fathers" 1 "Mothers", replace
label values sex sx

label variable howlng "Weekly Housework Hours"



****************************
save "raw_data.dta", replace 
****************************