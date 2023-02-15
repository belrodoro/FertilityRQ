/*****************************************************************

This do file carries out some validity checks on the relationship quality measures considered.

In order to justify the validity of these measures, we provide evidence in two different directions:

 1.	MEASURABILITY	-->	Individuals provide meaningful answers
 
	1.1. Correlation with objective measures:
	
		a.	Age and relationship tenure
		b.	Education and income
		c.	Happiness 
	
	1.2. Behavior prediction:
	
		a.	Divorce 
		b.	Marriage 
		c. 	Fertility 

 2. INTERPERSONAL COMPARABILITY	-->	There's a commonality in the conceprt
 
	2.1. Intra-couple correlation 


*****************************************************************/

clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}

cd "${samp}"



* =============================================================
* 	1.	MEASURABILITY: BEHAVIOR PREDICTION
* =============================================================

*..............................................................
* (a) Dissolution 
*..............................................................

use full_sample.dta, clear

sort ${unit}

* Indicator of separation between current and next period: 923 break-ups
gen aux = (transition==3 | transition==6) 

* Relationship quality in the wave prior separation 
gen pre_rq = rq1 if aux==1

* Subtract residuals from basic regression
qui reg rq1 ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
predict resid, residual

* Obtain empirical cdf
cumul resid, generate(cdf_res)

gen prediv_res = resid if aux==1
cumul prediv_res , generate(cdf_prediv_res)
	 
colorpalette lin fruits, locals
twoway (lpoly cdf_prediv_res prediv_res, lc(`Cherry')) (lpoly cdf_res resid, lc(gs11) lp(dash)), ///
	   legend(order(1 "Period before dissolution" 2 "Full sample") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   xtitle("RQ", size(medsmall)) xlabel(,labsize(small)) 
graph export "${graphs}/cdf_dissolution.png", replace
  
	 
* Kolmogrov-Smirnov test 
ksmirnov resid, by(aux)	 

* Smaller group             D     p-value  
* ---------------------------------------
* 0                    0.0000       1.000
* 1                   -0.2752       0.000
* Combined K-S         0.2752       0.000

* The 1st line tests that prediv==0 contains smaller values of RQ than prediv==1, which is not true looking at the p-value 
* The 2nd line tests that prediv==0 contains larger values of RQ than prediv==1, which is very much significant
* The last line is the combined test. The approximate p-values ksmirnov calculates are based on the five-term approximation of the asymptotic distributions derived by Smirnov (1933). The p-value indicates that the two distributions are significantly different.


	   
*..............................................................
* (b) Marriage 
*..............................................................
* Treated: individuals that marry next period 
* Control: full sample 

drop aux

* Indicator of marriage between current and next period: 1,150 marriages
gen aux = transition==2

* Relationship quality in the wave prior marriage 
gen premar_rq = rq1 if aux==1

* Subtract residuals from basic regression
*qui reg rq1 ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
*predict resid, residual

* Obtain empirical cdf
*cumul resid, generate(cdf_res)

gen premar_res = resid if aux==1
cumul premar_res , generate(cdf_premar_res)
	 
colorpalette lin fruits, locals
twoway (lpoly cdf_premar_res premar_res, lc(`Cherry')) (lpoly cdf_res resid, lc(gs11) lp(dash)), ///
	   legend(order(1 "Period before marriage" 2 "Full sample") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) ///
	   xtitle("RQ", size(medsmall)) xlabel(,labsize(small)) 
graph export "${graphs}/cdf_marriage.png", replace


*** Dissolution and marriage together:
colorpalette lin fruits, locals
twoway (lpoly cdf_res resid, lc(gs11) lp(dash)  ///
	   (clpolycdf_premar_res premar_res,  lc(`Blueberry') /// 
	   (clpolycdf_prediv_res prediv_res,  lc(`Cherry'), ///
	   legend(order(1 "Full sample" 2 "Period before" "marriage" 3 "Period before" "dissolution") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) yscale(outergap(*-3)) ///
	   xtitle("RQ", size(medsmall)) xlabel(,labsize(small))  xscale(outergap(*-3))
graph display, ysize(5) xsize(5)
graph export "${graphs}/cdf_transit.png", replace

	
* Kolmogrov-Smirnov test 
ksmirnov resid, by(aux)	 

* Smaller group             D     p-value  
* ---------------------------------------
* 0                    0.1257       0.000
* 1                   -0.0003       1.000
* Combined K-S         0.1257       0.000

* 1st line: pooled sample significantly below pre-marital sample  
* 2nd line: pooled sample NOT above pre-marital sample 
* The two distributions are significantly different.
	
	
	
*..............................................................
* (c) Fertility 
*..............................................................
* Treated: individuals about to have a child : 846 new parents
* Control: full sample 

use newparent_sample.dta, clear

global couple_c "ib1.tenure i.married"								

* Indicator of newparent, before having child
gen aux = newparent==1 & t_event<0

* Subtract residuals from basic regression
qui reg rq ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
predict resid, residual

* Obtain empirical cdf
cumul resid, generate(cdf_res)

gen prechild_res = resid if aux==1
cumul prechild_res , generate(cdf_prechild_res)
	 
colorpalette lin fruits, locals
twoway (lpoly cdf_prechild_res prechild_res, lc(`Grape')) (lpoly cdf_res resid, lc(gs11) lp(dash)), ///
	   legend(order(2 "New parent sample" 1 "Period before" "first child birth") pos(north) row(1) size(medsmall)) ///
	   ytitle("Empirical CDF", size(medsmall)) ylabel(,labsize(small)) yscale(outergap(*-3)) ///
	   xtitle("RQ", size(medsmall)) xlabel(,labsize(small)) xscale(outergap(*-3))
graph display, ysize(5) xsize(5)
graph export "${graphs}/cdf_child.png", replace
	 
	 
* Kolmogrov-Smirnov test 
ksmirnov resid, by(aux)	

* Smaller group             D     p-value  
* ---------------------------------------
* 0                    0.1000       0.000
* 1                   -0.0042       0.925
* Combined K-S         0.1000       0.000


twoway (kdensity resid) (kdensity resid if aux==1)

reg event rq ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)

* 1st line: pooled sample significantly below pre-child sample  
* 2nd line: pooled sample NOT above pre-child sample 
* The two distributions are significantly different.







* =============================================================
*	2.	INTERPERSONAL COMPARABILITY: WITHIN COUPLE 
* =============================================================

use full_sample.dta, clear

sort ${unit}

* Regress on observables 
reg rq1 ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
predict resid, residual


preserve 

* Create male sample 
tempfile males 

keep ${unit} sex partner rq1 resid 
keep if sex==0

rename (pidp sex rq1 resid) male_=
rename partner pidp
drop if pidp==.
duplicates drop ${unit}, force

save `males'

restore 

* Merge with female sample 
merge 1:1 ${unit} using `males', keep(3) nogen keepusing(male_sex male_rq1 male_resid)
label variable resid "Female RQ residuals"
label variable male_resid "Male RQ residuals"

keep if sex==1


** How good is male RQ predicting female RQ?
label variable rq1 "Woman's RQ"
label variable male_rq1 "Man's RQ"

eststo clear 
reg rq1 male_rq1 ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
eststo
estadd local controls "Age + Tenure"

esttab using "${tabs}/apx_couple_correlation.tex", b(3) se(3) r2(4) ///
	label drop(*age *tenure *wno) nobase ///
	scalar(controls) ///
	title("Regression of women's RQ on man's RQ and individual and couple characteristics") replace



** Correlate residuals by rank
xtile mres_pct = male_resid, n(100)
xtile fres_pct = resid	   , n(100)

egen mean_mres_pct = mean(mres_pct), by(fres_pct)
scatter mean_mres_pct fres_pct, ///
	ytitle("Man's average percentile", size(medsmall)) ylabel(0 (20) 100,labsize(small)) yscale(outergap(*-3)) ///
	xtitle("Woman's percentile", size(medsmall)) xlabel(0 (20) 100,labsize(small)) xscale(outergap(*-3))
graph display, ysize(5) xsize(5)
graph export "${graphs}/couple_correlation.png", replace

	
	
	
* Regress male on to female 
reg male_resid resid, nocons



egen cat_mres = cut(male_resid), group(100)
egen mean_fres = mean(resid), by(cat_mres)
scatter mean_fres male_resid



********

* Correlate 
correlate male_rq1 female_rq1
correlate res female_rq1

*(i) First graphic analysis: shows linear slope versus local polynomial approximations
twoway (scatter male_rq1 female_rq1 female_rq1) (lfit male_rq1 female_rq1)
twoway (scatter res female_rq1 female_rq1) (lfit res female_rq1)

	
*(ii) Non-parametric regression approach: 
reg male_rq1 female_rq1, nocons
reg res female_rq1, nocons

*(iii) Differences 
gen diff = male_rq1-female_rq1
gen diff2 = female_rq1-male_rq1

ttest diff==0
local ub = `r(mean)'+`r(se)'
local lb = `r(mean)'-`r(se)'
hist diff, xline(`r(mean)' `ub' `lb' 0)
