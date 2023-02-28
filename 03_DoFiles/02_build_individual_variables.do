/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-25
******************************************************************

This file builds the main dataset from Understanding Society

	Output:
	main_full_data.dta : Full individual level BHPS+UKHLS sample, w/ info on RQ and marital histories

	
===================================================================
				3 - Construct main outcomes  
===================================================================	


(2) Construct indices > factor analysis and preparation
	+ RQ 
	+ Gender role attitudes 
	+ Domestic division of labor

(3) Event study variables : event and t_event
		
(5) Parental leave variables
	
	
***************************************************
this is the point where we save individual_data.dta 
***************************************************

	
****************************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"

use "raw_data.dta", clear

*============================================================================
* 	1 - Construct indices 
*============================================================================

*---------------------------------------------------------------------------
* (1) Relationship Quality measure
*---------------------------------------------------------------------------

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


*---------------------------------------------------------------------------
* (2) Gender Norm Attitudes measure 
*---------------------------------------------------------------------------

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


*---------------------------------------------------------------------------
* (3) Domestic Labor Division index
*---------------------------------------------------------------------------

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


*============================================================================
*	2 - Construct event study variables 
*============================================================================

gsort ${unit}
rename nchild_dv nchild
// remember that nchild comes from time files (we take it from agepanel.dta). we are only looking at the changes in the number of children residing with you, so we are also counting parents whose adult child moved back with them --> this can all be built before

*----------------------------------------------------------------------
* Event: First child birth (single child)
*----------------------------------------------------------------------

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

* exclude couples with children from previous relationships
replace event = 0 if nchild>1 & event==1
ereplace newparent = max(event), by(pidp)

* correct double events when 2 waves in 1 year 
egen flag = sum(event), by(pidp)

gsort ${unit}
replace event = 0 if flag==2 & nchild==0 & nchild[_n+1]==1 & event[_n+1]==1                      // keep event when child appears 
replace event = 0 if event==1 & event[_n-1]==1 & pidp==pidp[_n-1] & nchild>0 & nchild[_n-1]>0    // if +1 children, keep first event 
replace event = 0 if event==1 & event[_n+1]==1 & pidp==pidp[_n+1] & nchild==0 & nchild[_n+1]==0  // if 0 children, keep second event 

* label variables 
label variable event "First child birth"
label variable newparent "Becomes parent during sample"

drop cum_nchild newmum newdad flag


*============================================================================
* 	3 - Parental leave variables
*============================================================================

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





*********************************
save individual_data.dta, replace 
*********************************


