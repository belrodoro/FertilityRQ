clear all
set more off

global path "E:\workdata\706209\Child penalty"

/***************************
Remember:
Women/Female/Mothers = w
Men/Male/Fathers     = m
****************************/
foreach q of numlist 1(1)7 {
foreach gender in women men {

	display " "
	display "`gender', `q'" 
	display " "

	use "$path\1Data\\`gender'`q'.dta", clear
	drop CHILD* pnrw pnrm partnerID CouplePartnerID FamilyID address origin arbgnr SpouseID
/*****************************************************************************************************************************************

								Construct important covariates for imputation procedure 

********************************************************************************************************************************************/
/*1) group women by cells of characteristics :
- generation 
- region of origin
- wealth / income at age25
- education at age 25 or highest education received
- parental characteristics ??
*/

* AGE AT FIRST CHILD AMONG WOMEN WHO END UP HAVING KIDS
	gen     agefirstchild =      yearoffirstchild -  generation 
	replace agefirstchild = . if yearoffirstchild == .
	gen logagefirstchild  =  log(agefirstchild)

* EDUCATION GROUPS: MAXIMUM EDUCATION LEVEL ACHIEVED
	gen educ_level = substr(OccupationE, 1, 2)
	destring educ_level, replace
	replace  educ_level=0 if educ_level==.
	bysort pnr: egen educ_25 = max(educ_level)
	replace          educ_25 = 0  if educ_25<30
	replace          educ_25 = 40 if educ_25>35

	drop OccupationE

* REGIONS OF ORIGIN 
	replace originRegion="u" if originRegion==""

* GENERATION : BY DECADES, UP TO TRUNCATION OF GENERATION IN 1972
	egen class_gen=cut(generation), at(1910 1930(10)1970 1972(10)2002)

* Rank individuals by quartile of income
		* for generation before 1955 = income in 1980
		* for geneation after 1955 = income at age 25
	gen     income25=loenmv if age==25 & generation>1955
	replace income25=loenmv if year==1980 & generation<=1955
	bysort pnr: egen income_25=mean(income25)
	bysort pnr: egen y1=min(year)
	keep if year==y1

* QUARTILES OF INCOME AT AGE25 (BY COHORT)
	cumul income_25, gen(cdf_inc25) by(generation)
	gen     quart_inc25=1 if cdf_inc25<.25
	replace quart_inc25=2 if cdf_inc25<.5 & cdf_inc25>=.25
	replace quart_inc25=3 if cdf_inc25<.75 & cdf_inc25>=.5
	replace quart_inc25=4 if cdf_inc25>=.75
	drop income25 income_25

* PARENTAL GENERATIONS AND WEALTH
	foreach parent in w m {
		egen    decade_w_`parent'=cut(generation`parent'), at(1910(10)1980)
		replace decade_w_`parent'=3000 if generation`parent'==.

		replace netwealth8095`parent'=0 if netwealth8095`parent'==.
		cumul   netwealth8095`parent', gen(cdf_wealth`parent')
		gen     quart_netwealth`parent'=1 if cdf_wealth`parent'<.25
		replace quart_netwealth`parent'=2 if cdf_wealth`parent'<.5 & cdf_wealth`parent'>=.25
		replace quart_netwealth`parent'=3 if cdf_wealth`parent'<.75 & cdf_wealth`parent'>=.5
		replace quart_netwealth`parent'=4 if cdf_wealth`parent'>=.75
		drop cdf_wealth`parent' 

		replace lonmv8095`parent'=0 if lonmv8095`parent'==.
		cumul   lonmv8095`parent', gen(cdf_loenmv`parent')
		gen     quart_loenmv`parent'=1 if cdf_loenmv`parent'<.25
		replace quart_loenmv`parent'=2 if cdf_loenmv`parent'<.5 & cdf_loenmv`parent'>=.25
		replace quart_loenmv`parent'=3 if cdf_loenmv`parent'<.75 & cdf_loenmv`parent'>=.5
		replace quart_loenmv`parent'=4 if cdf_loenmv`parent'>=.75
		drop cdf_loenmv`parent' 
	}


/*****************************************************************************************************************************************

								Allocating 0 vs positive completed fertility to truncated generations

********************************************************************************************************************************************/
* PREDICT PROBABILITY OF HAVING POSITIVE NUMBER OF CHILDREN USING BASELINE GENERATIONS
	cap gen child_complete = maxC > 0

	xi: reg   child_complete i.quart_inc25 i.educ_25 i.decade_w_w i.decade_w_m i.quart_netwealthw i.quart_netwealthm ///
			  i.quart_loenmvw i.quart_loenmvm i.originRegion generation ///
			  if generation>=1950 & generation<=1974

	predict probfertility, xb
	replace probfertility=. if maxC>0
	replace probfertility=. if generation<1975
	cumul   probfertility, gen(cdf_probfertility) by(generation)

	sum child_complete if generation>=1960 & generation<=1972
	local F_baseline=r(mean)

* LOOP BY GENERATION TO ALLOCATE TRUNCATED WOMEN/MEN TO TWO GROUPS: GROUP WITH CHILDREN BUT AFTER TRUNCATED AGE, AND GROUP WITHOUT CHILDREN.
	gen child_predict=.
	forval gen= 1973/2003{
		count if generation==`gen'
		local totgen=r(N)
		count if generation==`gen' & maxC==0
		local nchild0_gen=r(N)
		local nchild_gen=`totgen'-r(N)
		replace child_predict=1 if generation==`gen' & (1-cdf_probfertility)<=(`totgen'*`F_baseline'-`nchild_gen')/(`nchild0_gen')
	}

	gen     tilde_child_complete=child_complete
	replace tilde_child_complete=1 if child_predict==1

/*****************************************************************************************************************************************

								Allocating 1st child birth dates to women with 0 completed fertility

********************************************************************************************************************************************/
	gen agefirstchild_imputed=.

* WOMEN FROM GENERATIONS BEFORE 1972 (NO TRUNCATION)
	foreach  x in 0 35 40  {
		foreach y of numlist 1910 1930(10)1970 {
			sum logagefirstchild if maxC>0 & maxC!=. & educ_25==`x' & class_gen==`y'
			replace agefirstchild_imputed=exp(`r(mean)'+sqrt(`r(Var)')*invnorm(uniform())) if maxC==0 & educ_25==`x' & class_gen==`y' 
		}
	}
* WOMEN FROM GENERATIONS AFTER 1972 (DUE TO TRUNCATION, TAKE 1968 - 1972 AGE DISTRIBUTION AS BASELINE)
	foreach  x in 0 35 40  {
		foreach y of numlist 1972(10)2002 {
			sum logagefirstchild if maxC>0 & maxC!=. & educ_25==`x' & generation>=1968 & generation<=1972
			replace agefirstchild_imputed=exp(`r(mean)'+sqrt(`r(Var)')*invnorm(uniform())) if tilde_child_complete==0 & educ_25==`x' & class_gen==`y'
		}
	}

* BOUNDS ON IMPUTED AGE AT FIRST CHILD
	replace agefirstchild_imputed=min(agefirstchild_imputed, 50) if agefirstchild_imputed!=.
	replace agefirstchild_imputed=max(agefirstchild_imputed, 15) if agefirstchild_imputed!=.

* HISTOGRAMS
	histogram agefirstchild if generation>=1930 & generation<1973 & agefirstchild<=50 & agefirstchild>10, width(1)

	replace agefirstchild_imputed = floor(agefirstchild_imputed)

/*****************************************************************************************************************************************

								Graphs to compare distribution of age at first child

********************************************************************************************************************************************/
	preserve
		keep if agefirstchild!=.
		keep if generation>=1930 & generation<1973 & agefirstchild<=50 & agefirstchild>10
		gen compteur=1
		collapse (sum) compteur , by(agefirstchild)
		sum compteur
		gen frac=compteur/r(sum)
		sort agefirstchild
		save "$path\FinalGraphs\Figure A7\Tempdata\freq1", replace
	restore

	preserve
		keep if agefirstchild_imputed!=.
		keep if generation>=1930 & generation<1973 & agefirstchild_imputed<=50 & agefirstchild_imputed>10
		gen compteur=1
		collapse (sum) compteur , by(agefirstchild_imputed)
		rename agefirstchild_imputed agefirstchild
		rename compteur compteur_imputed
		sum compteur_imputed
		gen frac_imputed=compteur_imputed/r(sum)
		sort agefirstchild
		merge agefirstchild using "$path\FinalGraphs\Figure A7\Tempdata\freq1"
		sort agefirstchild 
		twoway (connected frac agefirstchild) (connected frac_imputed agefirstchild) , ///
		name(histo1, replace) graphregion(color(white)) ytitle(pdf) xtitle(Age at first child)
	restore

/*****************************************************************************************************************************************

								Saving

********************************************************************************************************************************************/
	keep pnr agefirstchild_imputed agefirstchild tilde_child_complete child_predict

	preserve
		use "$path\1Data\\`gender'`q'.dta", clear
		sort pnr
		save "$path\1Data\\`gender'`q'.dta", replace
	restore

	sort pnr
	merge pnr using "$path\1Data\\`gender'`q'.dta"
	save "$path\FinalGraphs\Figure A7\Tempdata\\`gender'_imputed_`q'", replace
}
}
/*****************************************************************************************************************************************

								The end

********************************************************************************************************************************************/