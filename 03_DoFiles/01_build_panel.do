/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file builds the main dataset from Understanding Society

	Output:
	main_full_data.dta : Full individual level BHPS+UKHLS sample, w/ info on RQ and marital histories
	couple_panel.dta   : Couple level panel with all info on main, for indiv w/ available partner

	
******************************************************************
****					INDIVIDUAL PANEL 					****
******************************************************************
===================================================================
			1 - Build and merge raw data files
===================================================================

Using raw Understanding Society files, build the following sets of information:

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
				
(2) Age-Period-Cohort panel 

	Input: indall.dta 
	Output: apc_file.dta > individuals w/ APC variables
	Variables of interest: 
		- APC: age year birthy 
		- Marriage status (unified): mastat

(3) Tenure

	Input: phistory_long.dta 
	Output: mhistory_indiv_year.dta > individuals w/ marital history

===================================================================
  2 - Use individual response info to fill marital histories in 
===================================================================

(1) Fill 'status' with individual information on marital status 
(2) Make corrections to tenure and partner 
(3) Convert transition into yearly variable
(4) Document first relationship

===================================================================
				3 - Construct main outcomes  
===================================================================	

(1) Clean and prepare general controls : sex, education, wave, controls 

(2) Construct indices > factor analysis and preparation
	+ RQ 
	+ Gender role attitudes 
	+ Domestic division of labor

(3) Event study variables : event and t_event
		
(4) Correct child variables : chind age, child sex, etc

(5) Parental leave variables
	
	
**************************************************
**************************************************
this is the point where we save main_full_data.dta 
**************************************************
**************************************************


******************************************************************
****					COUPLE PANEL 						****
******************************************************************
* 1. couple identifiers for individuals with observed partners 
* 2. one working dataset per gender 
* 3. merge gender files  
* 4. construct gender behavior
* 5. construct assortative mating
* 6. construct new parent variables

**************************************************
**************************************************
this is the point where we save couple_panel.dta 
**************************************************
**************************************************
	
****************************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

****************************************************************************
**************************** INDIVIDUAL PANEL ******************************
****************************************************************************

*============================================================================
*				1 - Build and merge raw data files 
*============================================================================

cd "${path}"

// * (1) Regressor panel: obtain RQ and individual characteristics
// quietly do "${dofiles}/aux_do/aux_build_relquality.do"
//
// * (2) Age-cohort-period panel: fill in all individuals' ages and periods 
// quietly do "${dofiles}/aux_do/aux_build_agepanel.do"
//
// * (3) Marital history panel: build panel of relationships
// quietly do "${dofiles}/aux_do/aux_build_tenure.do"
//
// * (4) Household relation history: identify partners
// quietly do "${dofiles}/aux_do/aux_build_relation_panel.do" 
//
// * (5) Information about first borns
// quietly do "${dofiles}/aux_do/aux_child_panel.do" 


*** Merge all raw datasets into an individual-year panel ***
cd "${samp}"

use "relquality.dta", clear 																		// characteristics
	merge 1:1 ${unit} using "apc_file.dta", nogen keepusing(age year birthy mastat sex nchild_dv newmum newdad)	// age, cohort, period 
	merge 1:1 ${unit} using "relation_panel.dta", nogen keepusing(partner)			// family matrix
		rename partner partner_fm
	merge n:1 pidp year using "mhistory_indiv_year.dta",  keep(1 3)									// marital history 
	merge n:1 pidp using "first_child.dta", keep(1 3) nogen									// first child data



*============================================================================
*		2 - Use individual response info to fill marital histories in 
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
* (2) Make corrections to tenure and partner 
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
*					3 - Construct relevant variables
*============================================================================

*---------------------------------------------------------------------------
* (1) Clean and prepare general controls 
*---------------------------------------------------------------------------

* a. Correct sex variable 
ereplace sex = max(sex), by(pidp)

* b. Build education variable 
*    It follows International Standard Classification of Education (ES-ISCED):  chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/viewer.html?pdfurl=https%3A%2F%2Fdam.ukdataservice.ac.uk%2Fmedia%2F262853%2Fdiscover_sqb_education_schneider.pdf&clen=548821&chunk=true page 16
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

* c. Wave indicators 
encode wave, gen(wno)

* d. Prepare controls
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


*---------------------------------------------------------------------------
* (2) Construct indices 
*---------------------------------------------------------------------------

** RELATIONSHIP QUALITY MEASURE **

* 1. transform items:	0 = most unhappy habits and increasing 
*						all responses will be increasing with rq 

* negative items : the more often the worse  
label define neg_rq 0 "all of the time" 1 "most of the time" 2 "more often than not" 3 "occasionally" 4 "rarely" 5 "never"
foreach var in screlpards screlparrg screlparar screlparir {
	replace `var' = `var' - 1
	label values `var' neg_rq
}
* positive items : the more often the better
label define pos_rq 0 "never" 1 "less than once a month" 2 "once or twice a month" 3 "once or twice a week" 4 "once a day" 5 "more often"
foreach var in screlparei screlparcd screlparwt {
	replace `var' = `var' - 1
	label values `var' pos_rq
}
label define kiss_rq 0 "never" 1 "rarely" 2 "occasionally" 3 "more often than not" 4 "most of the time" 5 "all of the time"
	replace screlparks = 7 - screlparks
	label values screlparks kiss_rq
label define happy_rq 0 "extremely unhappy" 1 "fairly unhappy" 2 "a little unhappy" 3 "happy" 4 "very happy" 5 "extremely happy" 6 "perfect"
	replace screlhappy = screlhappy - 1
	label values screlhappy happy_rq
label define int_rq 0 "none of them" 1 "very few of them" 2 "some of them" 3 "most of them" 4 "all of them"
	replace scparoutint = 5 - scparoutint
	label values scparoutint int_rq

* 2. run factor analysis 	
factor ${items} , pcf factors(5)
predict rq*

drop rq2 
rename rq1 rq 
label variable rq "RQ"


** GENDER NORM ATTITUDE MEASURE **

* 1. transform items: 	0 = least in line with gender equality and 4 = most in line with gender equality 
*						like this we have all responses alligned and the larger factor means more equality attitude 

* traditional items : agreeing goes against equality 
label define trad_gnorm 0 "strongly agree" 1 "agree" 2 "neither agree/disagree" 3 "disagree" 4 "strongly disagree"
foreach var in a b f {
	replace scopfam`var' = scopfam`var' - 1
	label values scopfam`var'  trad_gnorm
}
* non-traditional items : agreeing alligns with equality 
label define ntrad_gnorm 0 "strongly disagree" 1 "disagree" 2 "neither agree/disagree" 3 "agree" 4 "strongly agree"
foreach var in d h {
	replace scopfam`var' = 5 - scopfam`var'
	label values scopfam`var'  ntrad_gnorm
}

* 2. run factor analysis
factor scopfam*, pcf
predict gnorm*

drop gnorm2
rename gnorm1 gnorm
label variable gnor "Gender Role Attitudes"

* 3. mean response by individual and share of agree/disagree
foreach var in a b d f h {
	egen mean_gnorm`var' = mean(scopfam`var'), by(pidp)
	gen sh_gnorm`var' = mean_gnorm`var'>=3	
}
label variable sh_gnorma "A pre-school child is likely to suffer if his or her mother works."
label variable sh_gnormb "All in all, family life suffers when the woman has a full-time job."
label variable sh_gnormd "Both the husband and wife should contribute to the household income."
label variable sh_gnormf "A husband's job is to earn money, a wife's job is to look after the home and family."
label variable sh_gnormh "Employers should make special arrangements to help mothers combine jobs and childcare."

drop mean_* 


** GENDER BEHAVIOR MEASURE: DOMESTIC LABOR DIVISION **

* 0. some variables are coded differently on the last wave :

* general variables :
*		1	Always me
*		2	Usually me
*		3	Me and partner about equally
*		4	Usually partner
*		5	Always partner
*		6	Always or usually other person in the household
*		7	Always or usually someone not living in the household
*		8	Not applicable	
foreach var in hubuys hudiy hufrys huiron humops hupots {
	replace `var' = 1  if `var'==2 & panel=="UKHLS" & wave=="l"						// 1  mostly self 
	replace `var' = 2  if `var'==4 & panel=="UKHLS" & wave=="l"						// 2  mostly partner
	replace `var' = 2  if `var'==5 & panel=="UKHLS" & wave=="l"						// 3  shared
	replace `var' = 4  if `var'==7 & hupayhswrk==1 & panel=="UKHLS" & wave=="l"		// 4  paid help
	replace `var' = 97 if `var'==6 & panel=="UKHLS" & wave=="l"						// 99 other
	replace `var' = 97 if `var'==7 & panel=="UKHLS" & wave=="l"
	replace `var' = .  if `var'==8 & panel=="UKHLS" & wave=="l"
} 
* new child variables : hucbed hucdress hucferry huchomework hucplay hucunwell
*	define husits depending on which are with most frequency : 1 me, 2 you, 3 both 
gen aux1 = hucbed==1 | hucbed==2
gen aux2 = hucbed==4 | hucbed==5 
gen aux3 = hucbed==3
foreach var of varlist hucdress hucferry huchomework hucplay hucunwell {
	replace aux1 = aux1 + 1 if `var'==1 | `var'==2
	replace aux2 = aux2 + 1 if `var'==4 | `var'==5
	replace aux3 = aux3 + 1 if `var'==3
}
gen aux_husits = 1 if aux1>aux2 & aux1>aux3
replace aux_husits = 2 if aux2>aux1 & aux2>aux3
replace aux_husits = 3 if aux3>aux2 & aux3>aux1

replace aux_husits = 1 if aux_husits==. & aux1>aux2		// if there is a tie with shared, whoever is higher
replace aux_husits = 2 if aux_husits==. & aux1<aux2
replace aux_husits = 3 if aux_husits==. & aux1==aux2

gen husits_dv = husits
replace husits_dv = aux_husits if panel=="UKHLS" & wave=="l"
label values husits_dv b_husits
label variable husits_dv "who is responsible for childcare"

drop aux*

* other variables : hucar (d, f, h, j) huruns (d, h)

* 1. generate variable with 3 hh categories
// i removed "husits_dv hudiy hupots" from global, like this we have also info on BHPS 
label define cat 1 "Traditional" 2 "Egalitarian" 3 "Counter-traditional", replace
foreach var in huboss hudiy hupots hubuys hufrys huiron humops husits_dv {
	gen `var'_cat = 0
	label values `var'_cat cat
}
* traditionally male 	
foreach var of varlist huboss hudiy hupots {	
	replace `var'_cat = 1 if (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace `var'_cat = 3 if  (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace `var'_cat = 2 if `var'==3
	
	replace `var'_cat = . if `var'==.
}
* traditionally female 
foreach var of varlist hubuys hufrys huiron humops husits_dv {	
	replace `var'_cat = 1 if (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace `var'_cat = 3 if (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace `var'_cat = 2 if `var'==3

	replace `var'_cat = . if `var'==.
}

* 2. classification into traditional and egalitarian
foreach var of varlist *_cat {
	gen `var'2_id = `var' == 2 | `var'==3	
	replace `var'2_id = . if `var'==.
}
rename *_cat2* **


*---------------------------------------------------------------------------
* (3) Event study
*---------------------------------------------------------------------------

// use "${samp}/apc_file.dta", clear

gsort ${unit}
rename nchild_dv nchild
// remember that nchild comes from time files (we take it from agepanel.dta). we are only looking at the changes in the number of children residing with you, so we are also counting parents whose adult child moved back with them --> this can all be built before


* Event: First child birth (single child)

* 1. using births in family matrix: trust ch_birthy over nchild (verified)
gen event = year==ch_birthy & ch_birthy!=.		
egen newparent = max(event), by(pidp)			// track new parents
	

* 2. first occurrence of children at home
bysort pidp (panel wave) : gen cum_nchild = sum(nchild) 

replace event = 1 if (newparent==0 & cum_nchild>0 & cum_nchild[_n-1]==0 & pidp[_n-1]==pidp[_n])
ereplace newparent = max(event), by(pidp)


* 3. variable indicating biological parent of new baby
replace event = 1 if newparent==0 & (newmum==1 |newdad==1) & cum_nchild<2  
ereplace newparent = max(event), by(pidp)


* exclude events before the age of 18 and after 40 for women - 45 for men
replace event = 0 if age<18 | (age>40 & sex==2) | (age>45 & sex==1)
ereplace newparent = max(event), by(pidp)

* label variables 
label variable event "First child birth"
label variable newparent "Becomes parent during sample"

drop cum_nchild newmum newdad 


* Event-time: leads and lags around event setting event to 0 (use wave)
gen t_event = 0 if event==1

gsort ${unit}
replace t_event = t_event[_n-1] + 1 if t_event==. & pidp==pidp[_n-1]

gsort pidp -panel -wave
replace t_event = t_event[_n-1] - 1 if t_event==. & pidp==pidp[_n-1]

gsort ${unit}

label variable t_event "Event-time: periods around event"


* First child sex
replace ch_sex = ch_sex - 1 
label define ba_asex 0 "Boy" 1 "Girl", replace 




*---------------------------------------------------------------------------
* (4) Parental leave variables
*---------------------------------------------------------------------------

* unique value of matlv: when event 
foreach var of varlist matlv* {

	gen aux = `var' if nchild==1
	egen aux2 = nvals(aux), by(pidp)

	ereplace aux = min(aux), by(pidp)
	replace aux = . if event!=1

	recode aux (2 = 0) (3 = 1)

	gen parent_`var' = aux if event==1

	drop aux*
}
rename *_mat* **

* date variables 
foreach xx in st end {
	gen parent`xx'dt = mdy(parentlv`xx'm, parentlv`xx'd, parentlv`xx'y4)
	format parent`xx'dt %d
}

gen parentlvdur = parentenddt - parentstdt
replace parentlvdur = 0 if parentlv==0

* label variables 
label variable parentlv 	"Parental leave after 1st child birth"
label variable parentlvdur 	"Parental leave duration"



********************************
********************************
save main_full_data.dta, replace 
********************************
********************************



****************************************************************************
****************************** COUPLE PANEL ********************************
****************************************************************************

use main_full_data.dta, clear  
sort ${unit}


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
label variable gbeh "Domestic Labor Division"

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

gen dadlv = newparent==1 & m_parentlv==1
ereplace dadlv = max(dadlv), by(cidp)
 
 

******************************
save couple_panel.dta, replace 
******************************

use couple_panel.dta, clear 






