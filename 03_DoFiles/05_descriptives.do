/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-01-25
******************************************************************

This file produced basic descriptives 

===================================================================
					SUMMARY STATISTICS
===================================================================

Constructs a table summarizing following characteristics:

	(1) Individual characteristics:
		age, sex, tertiary education, employment status, montly gross personal income, living in urban areas
	
	(2) Couple characteristics: 
		tenure, married, any children in hh
				

===================================================================
				DISTRIBUTION OF RQ
===================================================================	

Describes RQ measure separately for:

	(1) Full sample, split by gender

	(2) New-parent sample, split by gender

===================================================================
				FACTOR ANALYSIS
===================================================================

Carries out factor analysis in the full data and captures factor loadings.
Splits the factor analysis by gander and checks for differences in loadings.

	
......................................................................*/

clear all 
set more off 


if "`c(username)'"=="belen" {
	do "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles\00_globals.do"
}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}


cd "${samp}"


// eststo r1: xtreg mb_ca2y $controls diinc_3_rel, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// eststo r2: xtreg mb_ca2y $controls diinc_3_rel if ae == 1, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// eststo r3: xtreg mb_ca2y $controls diinc_1_rel, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// eststo r4: xtreg mb_ca2y $controls diinc_1_rel if ae == 1, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// eststo r5: xtreg mb_ca2y $controls diinc_gini_rel, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// eststo r6: xtreg mb_ca2y $controls diinc_gini_rel if ae == 1, fe vce(rob)
// qui estadd local Sample "Full"
// qui estadd local FE 	"Yes"
// esttab r1 r2 r3 r4 r5 r6, star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) se(%9.3f) mtitles stats(Sample FE r2 N, fmt(2 2 2 0) labels("Sample" "Country FE" "R-squared" "Observations")) rename(diinc_3_rel "Income inequality" diinc_1_rel "Income inequality" diinc_gini_rel "Income inequality") keep("Income inequality")

*============================================================================
* 1. sample summary statistics
*============================================================================
use newparent_sample.dta, clear 

* recode into percentages 
recode sex (1=100) 
recode tertiary (1=100)
recode employ (1=100)
recode urban_dv (1=100)

* label variables 
label variable age "Age"
label variable sex "Women (\%)"
label variable tertiary "College educated (\%)"
label variable employ "Employed (\%)"
label variable fimngrs_dv "Gross monthly income"
label variable urban_dv "In urban areas (\%)"

* summary - individual characteristics
eststo se_newpar: estpost summarize age sex tertiary employ fimngrs_dv urban_dv 

* recode into percentages 
recode married (1=100)
recode ch1 (1=100)

* label variables 
label variable tenure "Tenure"
label variable married "Married (\%)"
label variable ch1 "At least one child (\%)"

* summary - couple characteristics 
eststo ma_newpar: estpost summarize tenure married ch1


// top panel 
esttab se_newpar using "${tabs}/sumstats.tex", ///
	prehead("\begin{singlespace}""\begin{table}[h!]\centering\footnotesize"" \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} ""\caption{Summary statistics for the different samples} ""\label{tab:sumstat} ""\begin{tabular}{l*{2}{c}} ""\hline\hline") ///
	posthead("\hline \\ \multicolumn{3}{l}{\textit{Panel A: Individual characteristics}} \\") ///
	fragment ///
	mtitles("Full sample" "Child sample") ///
	main(mean) aux(sd) label replace

// bottom panel 
esttab ma_newpar using "${tabs}/sumstats.tex", ///
	posthead("\hline \\ \multicolumn{3}{l}{\textit{Panel B: Couple characteristics}} \\ ") ///
	fragment ///
	append ///
	nomtitles nonumbers ///
	main(mean) aux(sd) label ///
	prefoot("\hline") ///
	postfoot("\hline\hline ""\multicolumn{3}{l}{\footnotesize Standard errors in parentheses.}""\end{tabular} ""\end{table} ""\end{singlespace}") 


*============================================================================
* 2. regression coefficients
*============================================================================
use newparent_sample.dta, clear 

eststo det: reg rq ${indiv_c} ${couple_c} ib2.wno, vce(cluster cidp)
estadd local controls "Age + Tenure + Wave"

esttab det using "${tabs}/determinants.tex", b(3) se(3) r2(4) ///
	label drop(*age *tenure *wno) nobase ///
	mlabels("RQ") scalar(controls) ///
	title("Regression of RQ on individual and couple characteristics") replace

*============================================================================
* 3. distribution of RQ
*============================================================================

** Full dataset 
use full_sample.dta, clear  

sum rq if sex==0
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==0, bin(40) name(men, replace) ///
	ytitle("", size(tiny)) ylabel(0(.2).6, labs(medsmall)) ///
	xtitle("", size(tiny)) xlabel(, labs(medsmall)) ///
	text(0.5 -5 "Mean: `mean'" "SD: 0.95", size(medium) just(left)) ///
	title("Male", size(medlarge)) nodraw
qui sum rq if sex==1
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==1, bin(40) name(women, replace) ///
	yscale(off) ylabel(0(.2).6, labs(medsmall)) ///
	xtitle("", size(tiny)) xlabel(, labs(medsmall)) ///
	text(0.5 -5 "Mean: `mean'" "SD: `sd'", size(medium) just(left)) ///
	title("Female", size(medlarge)) nodraw
graph combine men women, l1("Density", size(medsmall)) b1("Relationship Quality", size(medsmall)) title("Panel A - Full data", size(medium)) name(all, replace)

** Sample
use newparent_sample.dta, clear

sum rq if sex==0
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==0, bin(40) name(men, replace) ///
	ytitle("", size(tiny)) ylabel(0(.2).6, labs(medsmall)) ///
	xtitle("", size(tiny)) xlabel(, labs(medsmall)) ///
	text(0.5 -5 "Mean: `mean'" "SD: 0.94", size(medium) just(left)) ///
	title("Male", size(medlarge)) nodraw
qui sum rq if sex==1
	local mean = round(`r(mean)', 0.01)
	local sd   = round(`r(sd)', 0.01)
hist rq if sex==1, bin(40) name(women, replace) ///
	yscale(off) ylabel(0(.2).6, labs(medsmall)) ///
	xtitle("", size(tiny)) xlabel(, labs(medsmall)) ///
	text(0.5 -5 "Mean: `mean'" "SD: `sd'", size(medium) just(left)) ///
	title("Female", size(medlarge)) nodraw
graph combine men women, l1("Density", size(medsmall)) b1("Relationship Quality", size(medsmall)) title("Panel B - New parent sample", size(medium)) name(newp, replace)

graph combine all newp, row(2)
graph export "${graphs}/rq_dist.png", replace

// sum rq if sex==0
// 	local mean = round(`r(mean)', 0.01)
// 	local sd   = round(`r(sd)', 0.01)
// hist rq if sex==0, bin(40) color(${Blueberry}) ///
// 	ytitle("density", size(medium)) ylabel(0(.2).6, labs(medsmall))  yscale(outergap(*-3)) ///
// 	xtitle("RQ", size(medium)) xlabel(, labs(medsmall))  xscale(outergap(*-3)) ///
// 	text(0.5 -5 "Mean: 0.04" "SD: 0.95", size(medium) just(left))
// graph display, ysize(5) xsize(5)
// graph export "${graphs}/Presentation/dist_m.png", replace
//	
// qui sum rq if sex==1
// 	local mean = round(`r(mean)', 0.01)
// 	local sd   = round(`r(sd)', 0.01)
// hist rq if sex==1, bin(40) color(${Tangerine}) ///
// 	ytitle("density", size(medium)) ylabel(0(.2).6, labs(medsmall))  yscale(outergap(*-3)) ///
// 	xtitle("RQ", size(medium)) xlabel(, labs(medsmall))  xscale(outergap(*-3)) ///
// 	text(0.5 -5 "Mean: -0.05" "SD: `sd'", size(medium) just(left))
// graph display, ysize(5) xsize(5)
// graph export "${graphs}/Presentation/dist_w.png", replace


*---------------------------------
* People with and without children
*---------------------------------


*============================================================================
* 4. factor analysis
*============================================================================

*---------------------------------------------------------------------------
* 4.1. relationship quality
*---------------------------------------------------------------------------
use main_full_data.dta, clear  
sort ${unit}

drop rq*

* Recode negative items on a decreasing scale: 
replace screlpards = 7-screlpards
replace screlparrg = 7-screlparrg
replace screlparar = 7-screlparar
replace screlparir = 7-screlparir

* Recode positive items on a increasing scale:
replace screlparks = 7-screlparks
replace scparoutint = 5-scparoutint 

* Factor scores for all
factor ${items}, pcf factors(1)

predict rq*
matrix list r(scoef)

matrix m_all = r(scoef)[1..10,1]

* Factor scores for men
factor ${items} if sex==0, pcf factors(5)

predict rq_men*
matrix list r(scoef)

matrix m_men = r(scoef)[1..10,1]

* Factor scores for women
factor ${items} if sex==1, pcf factors(5)

predict rq_women*
matrix list r(scoef)

matrix m_women = r(scoef)[1..10,1]

* Put together and plot
matrix M = (m_all , m_men , m_women)

matrix rownames M = "consider splitting" "regret getting married" "quarrel" "get on each other's nerves" "work together on a project" "stimulating exchange of ideas" "calmly discuss something" "kiss" "engage in outside interests" "degree of happiness" 
matrix colnames M = "scores" "only men" "only women"

matrix list M

svmat M		// convert matrix into dataset
keep M*
keep if M1!=.
de


egen item = seq()
label define it 1 "consider splitting" 2 "regret getting married" 3 "quarrel" 4 "get on each other's nerves" 5  "work together on a project" 6 "stimulating exchange of ideas" 7 "calmly discuss something" 8 "kiss" 9 "engage in outside interests"  10 "degree of happiness" 
label values item it

order item


*---------------------------------------------------------------------------
* 4.2. domestic labor division
*---------------------------------------------------------------------------

use couple_panel.dta, clear 
drop gbeh

* factor analysis
factor  m_huboss_id m_hubuys_id m_hufrys_id m_huiron_id m_humops_id ///
		f_huboss_id	f_hubuys_id f_hufrys_id f_huiron_id f_humops_id	
		// explains .5728 variation without pcf 

predict gbeh*

* store results
matrix list r(scoef)
matrix m_men   = r(scoef)[1..5, 1]
matrix m_women = r(scoef)[6..10, 1]

matrix M = (m_men , m_women)

matrix colnames M = "men response" "women response"

matrix list M

	
*---------------------------------------------------------------------------
* 4.3. fertility rate
*---------------------------------------------------------------------------
import delimited using "C:\Users\Olatz\Downloads\DP_LIVE_22012023133001280.csv", varnames(1) clear
keep if location=="GBR" | location=="EU" | location=="OAVG" 

twoway 	(connected value time if location=="OAVG", lcolor(${Grape}) lpattern(dash_dot) mcolor(${Grape}) msymbol("sh")) ///
		(connected value time if location=="EU"  , lcolor(${Blueberry}) lpattern(dash_dot) mcolor(${Blueberry}) msymbol("sh")) ///
		(connected value time if location=="GBR", lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")), ///
		ytitle("Fertility Rate", size(medsmall)) ylabel(#5, labsize(small)) ///
		xtitle("Year", size(medsmall)) xlabel(#6, labsize(small))   ///
		legend(order(1 "OECD" 2 "European Union" 3 "United Kingdom") pos(12) r(1) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility.png", replace


*============================================================================
* 5. responses to domestic labor division
*============================================================================

use newparent_sample.dta, clear

* traditionally male 
foreach var of varlist huboss hudiy hupots {	
	* traditional hh
	gen sh_`var'_trad = (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace sh_`var'_trad = . if `var'==.

	* counter traditional hh
	gen sh_`var'_count = (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace sh_`var'_count = . if `var'==.

	* egalitarian hh
	gen sh_`var'_egal = `var'==3
	replace sh_`var'_egal = . if `var'==.
}

* traditionally female 
foreach var of varlist hubuys hufrys huiron humops husits_dv {	
	* traditional hh
	gen sh_`var'_trad = (`var'==1 & sex==1) | (`var'==2 & sex==0)
	replace sh_`var'_trad = . if `var'==.

	* counter traditional hh
	gen sh_`var'_count = (`var'==1 & sex==0) | (`var'==2 & sex==1)
	replace sh_`var'_count = . if `var'==.

	* egalitarian hh
	gen sh_`var'_egal = `var'==3
	replace sh_`var'_egal = . if `var'==.
}
	
label variable sh_huboss_egal "household financial decisions"
label variable sh_hubuys_egal "who does the grocery shopping"
label variable sh_hudiy_egal "who does the diy jobs"
label variable sh_hufrys_egal "who does the cooking"
label variable sh_huiron_egal "who does the washing/ironing"
label variable sh_humops_egal "who does the cleaning"
label variable sh_hupots_egal "who does the gardening"
label variable sh_husits_dv_egal "who is responsible for childcare"

eststo trad_gbeh: 	estpost summarize *_trad
eststo egal_gbeh:   estpost summarize *_egal
eststo count_gbeh:  estpost summarize *_count

use "newparent_couples.dta", clear
eststo agree_gbeh:	estpost summarize *_ag


esttab *_gbeh using "${tabs}/gbeh.tex", ///
	rename(sh_huboss_trad sh_huboss_egal sh_hubuys_trad sh_hubuys_egal sh_hudiy_trad sh_hudiy_egal sh_hufrys_trad sh_hufrys_egal sh_huiron_trad sh_huiron_egal sh_humops_trad sh_humops_egal sh_hupots_trad sh_hupots_egal sh_husits_dv_trad sh_husits_dv_egal sh_huboss_count sh_huboss_egal sh_hubuys_count sh_hubuys_egal sh_hudiy_count sh_hudiy_egal sh_hufrys_count sh_hufrys_egal sh_huiron_count sh_huiron_egal sh_humops_count sh_humops_egal sh_hupots_count sh_hupots_egal sh_husits_dv_count sh_husits_dv_egal huboss_ag sh_huboss_egal hubuys_ag sh_hubuys_egal hufrys_ag sh_hufrys_egal huiron_ag sh_huiron_egal humops_ag sh_humops_egal hudiy_ag sh_hudiy_egal hupots_ag sh_hupots_egal husits_dv_ag sh_husits_dv_egal ) ///
	mtitles("traditional" "egalitarian" "counter- trad." "agree") ///
	main(mean) aux(sd) label replace

*============================================================================
* 	6. percentage responses to each gender role attitudes 
*============================================================================

foreach samp in main_full_data newparent_sample {
	
	use "`samp'.dta", clear  
	sort ${unit}


	* create tables 
	eststo all_gnorm1: estpost summarize sh_gnorma sh_gnormb sh_gnormf
	eststo ma_gnorm1:  estpost summarize sh_gnorma sh_gnormb sh_gnormf if sex==0
	eststo fe_gnorm1:  estpost summarize sh_gnorma sh_gnormb sh_gnormf if sex==1

	eststo all_gnorm2: estpost summarize sh_gnormd sh_gnormh
	eststo ma_gnorm2:  estpost summarize sh_gnormd sh_gnormh if sex==0
	eststo fe_gnorm2:  estpost summarize sh_gnormd sh_gnormh if sex==1
	
	eststo all_gnorm3: estpost summarize gnorm
	eststo ma_gnorm3:  estpost summarize gnorm if sex==0
	eststo fe_gnorm3:  estpost summarize gnorm if sex==1


	* gender differences 
	foreach var in a b d f h {
		reg sh_gnorm`var' i.sex , vce(cluster pidp)
	}
	reg gnorm i.sex, vce(cluster pidp)

	* i write this by hand in the table 

	if "`samp'"=="main_full_data" {
		local name = "full"
	}
	if "`samp'"=="newparent_sample" {
		local name = "new"
	}

	// top panel 
	esttab *_gnorm1 using "${tabs}/gnorm_`name'.tex", ///
		prehead("\begin{singlespace}""\begin{table}[h!]\centering\footnotesize"" \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} ""\caption{Summary statistics for gender role attitudes.} ""\label{tab:sumgnorm} ""\begin{tabular}{l*{3}{c}} ""\hline\hline") ///
		posthead("\hline \\ \multicolumn{4}{l}{\textit{Disagree / strongly disagree with}} \\") ///
		fragment ///
		mtitles("pooled" "men" "women") ///
		main(mean) aux(sd) label replace

	// middle panel 
	esttab *_gnorm2 using "${tabs}/gnorm_`name'.tex", ///
		posthead("\hline \\ \multicolumn{4}{l}{\textit{Agree / strongly agree with}} \\ ") ///
		fragment ///
		append ///
		nomtitles nonumbers ///
		main(mean) aux(sd) label ///
		prefoot("\hline") 

	// bottom panel 
	esttab *_gnorm3 using "${tabs}/gnorm_`name'.tex", ///
		posthead("\hline \\  ") ///
		fragment ///
		append ///
		nomtitles nonumbers ///
		main(mean) aux(sd) label ///
		prefoot("\hline") ///
		postfoot("\hline\hline ""\end{tabular} ""\end{table} ""\end{singlespace}") 
		
}


/*
* Plotting the scores *

colorpalette lin fruits, global opacity(70)
graph hbar (mean) M1 M2 M3 , over(item) ///
	bar(1, color(gray%70)) bar(2, color(${Blueberry})) bar(3, color(${Cherry})) ///
	ytitle("Score", size(medsmall)) ylabel(,labsize(small)) ///
	legend(order(1 "All indiv." 2 "Male" 3 "Female") size(small) pos(12) r(1)) 
colorpalette lin fruits, global opacity(70)

graph hbar (mean) M1 , over(item) ///
	bar(1, color(gray%70)) ///
	ytitle("Score", size(medsmall)) ylabel(,labsize(small)) ///
	legend(order(1 "All indiv." 2 "Male" 3 "Female") size(small) pos(12) r(1)) 
// overall most important regret getting married, but no large differences 

colorpalette lin fruits, global opacity(70)
graph hbar (mean) M2 , over(item) ///
	bar(1, color(${Blueberry}))  ///
	ytitle("Score", size(medsmall)) ylabel(,labsize(small)) ///
	legend(order(1 "All indiv." 2 "Male" 3 "Female") size(small) pos(12) r(1)) 
// most important regret getting married, but no large differences 
	
colorpalette lin fruits, global opacity(70)
graph hbar (mean) M3 , over(item) ///
	 bar(1, color(${Cherry})) ///
	ytitle("Score", size(medsmall)) ylabel(,labsize(small)) ///
	legend(order(1 "All indiv." 2 "Male" 3 "Female") size(small) pos(12) r(1)) 
// most important engage in outside activities, but no large differences 
	

* Differences in scores
gen diff = abs(M2) - abs(M3)

graph hbar (mean) diff , over(item)
// only calmly discussing and kissing have larger scores for women. largest differences in consider splitting, regret marrying and working together in project 
