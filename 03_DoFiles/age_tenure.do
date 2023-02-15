/*****************************************************************

Project: Children and Relationship Quality
Authors: Belén Rodríguez Moro and Olatz Román Blanco

THIS FILE COMPUTES THE AGE AND TENURE PROFILES ON RQ
	
	
LAST MODIFIED: 14/01/2023
******************************************************************/
cd "${samp}"

use child_panel.dta, clear  
drop flag* sex

keep if ch_num == 1
drop ch_num 

rename (ch_birthy) (yearoffirstchild)

merge 1:n pidp using "main_full_data.dta", keep(2 3) nogen keepusing(year age birthy sex rq newparent tertiary country fimnnet_dv)  
sort pidp year

rename (birthy tertiary country fimnnet_dv) (generation educ_25 originRegion loenmv)

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

* REGIONS OF ORIGIN 

* GENERATION : BY DECADES, UP TO TRUNCATION OF GENERATION IN 1972

* Rank individuals by quartile of income
		* for generation before 1955 = income in 1980
		* for geneation after 1955 = income at age 25
	gen     income25=loenmv if age==25
	bysort pidp: egen income_25=mean(income25)
	egen aux = max(year), by(pidp)
	replace income25=loenmv if income25==. & year==aux
	ereplace income_25 = max(income25), by(pidp)
	bysort pidp: egen y1=min(year)
	keep if year==y1

* QUARTILES OF INCOME AT AGE25 (BY COHORT)
	cumul income_25, gen(cdf_inc25) by(generation)
	gen     quart_inc25=1 if cdf_inc25<.25
	replace quart_inc25=2 if cdf_inc25<.5 & cdf_inc25>=.25
	replace quart_inc25=3 if cdf_inc25<.75 & cdf_inc25>=.5
	replace quart_inc25=4 if cdf_inc25>=.75
	drop income25 income_25






* individuals that never become parents 
gen neverparent = (age>40 & ch_birthy1==.)
ereplace neverparent = max(neverparent), by(pidp)

* truncated generations

egen id = seq() if ch_birthy1==. & newparent==1, by(pidp)

/*****************************************************************************************************************************************

								Allocating 0 vs positive completed fertility to truncated generations

********************************************************************************************************************************************/
* PREDICT PROBABILITY OF HAVING POSITIVE NUMBER OF CHILDREN USING BASELINE GENERATIONS
	cap gen child_complete = yearoffirstchild!=.

	xi: reg   child_complete i.quart_inc25 i.educ_25 ///
			  i.originRegion generation

	predict probfertility, xb
	replace probfertility=. if child_complete==1
	cumul   probfertility, gen(cdf_probfertility) by(generation)

	sum child_complete if generation>=1960 & generation<=1972
	local F_baseline=r(mean)

* LOOP BY GENERATION TO ALLOCATE TRUNCATED WOMEN/MEN TO TWO GROUPS: GROUP WITH CHILDREN BUT AFTER TRUNCATED AGE, AND GROUP WITHOUT CHILDREN.
	gen child_predict=.
	forval gen= 1973/2003{
		count if generation==`gen'
		local totgen=r(N)
		count if generation==`gen' & child_complete==0
		local nchild0_gen=r(N)
		local nchild_gen=`totgen'-r(N)
		replace child_predict=1 if generation==`gen' & (1-cdf_probfertility)<=(`totgen'*`F_baseline'-`nchild_gen')/(`nchild0_gen')
	}

	gen     tilde_child_complete=child_complete
	replace tilde_child_complete=1 if child_predict==1


keep if age>=25 & age<=70
keep if tenure<=40

*-----------------------------------------------
* For the full sample 
*-----------------------------------------------
xtreg rq ${indiv_c} ${couple_c} ib2.wno, fe vce(cluster cidp) nonest

colorpalette lin fruits, local 
coefplot, keep(*age) vertical recast(connected) msymbol("sh") lpattern(solid) lc(`Blueberry') mc(${Blueberry}) noci ///
		coeflabel(26.age="26" 27.age=" " 28.age=" " 29.age=" " 30.age="30" 31.age=" " 32.age=" " 33.age=" " ///
				  34.age=" " 35.age="35" 36.age=" " 37.age=" " 38.age=" " 39.age=" " 40.age="40" 41.age=" " ///
				  42.age=" " 43.age=" " 44.age=" " 45.age="45" 46.age=" " 47.age=" " 48.age=" " 49.age=" "  ///
				  50.age="50" 51.age=" " 52.age=" " 53.age=" " 54.age=" " 55.age="55" 56.age=" " 57.age=" " ///
				  58.age=" " 59.age=" " 60.age="60" 61.age=" " 62.age=" " 63.age=" " 64.age=" " 65.age="65" ///
				  66.age=" " 67.age=" " 68.age=" " 69.age=" " 70.age="70" , labsize(small)) ///
		yline(0) ylabel(-1.6(.4)1.6,labsize(small)) ytitle("Age Profile", size(medsmall)) yscale(titlegap(*-20) outergap(*-3)) ///
		xtitle("Age", size(medsmall)) xscale(outergap(*-3)) ///
	    legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/age_effect.png", replace
		
colorpalette lin fruits, local 
coefplot, keep(*tenure) vertical recast(connected) msymbol("sh") lpattern(solid) lc(`Blueberry') mc(${Blueberry}) noci ///
		yline(0) ylabel(-1.6(.4)1.6,labsize(small)) ytitle("Tenure Profile", size(medsmall)) yscale(titlegap(*-20) outergap(*-3))  ///
		xlabel(#10,labsize(small)) xtitle("Tenure", size(medsmall)) xscale(outergap(*-3)) ///
		legend(off) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/tenure_effect.png", replace
		

