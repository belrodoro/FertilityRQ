*=======================================================================
* DESCRIPTIVES: 
*	1. Did it increases the number of fathers taking the leave?
*	2. Did it increase the duration of paternal leaves?
*=======================================================================

use newparent_sample.dta, clear
keep if panel=="UKHLS"


drop if year < 2010 | year > 2020

* plot shares 
graph bar parentlv, over(sex) over(year) asyvars ///
		  bar(1, color(${Blueberry})) bar(2, color(${Tangerine})) ///
		  ytitle("Share taking parental leave", size(medium)) ///
		  legend(order(1 "Fathers" 2 "Mothers") pos(12) r(1) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/leave_1.png", replace

graph bar parentlvdur, over(sex) over(year) asyvars ///
		  bar(1, color(${Blueberry})) bar(2, color(${Tangerine})) ///
		  ylabel(0(70)280, labsize(small)) ytitle("Mean duration of leave, days", size(medium)) ///
		  legend(order(1 "Fathers" 2 "Mothers") pos(12) r(1) size(medsmall))
graph display, ysize(5) xsize(5)
graph export "${graphs}/leave_2.png", replace



*=======================================================================
* AMOUNT OF HOUSEWORK HOURS
* 
*=======================================================================

use newparent_sample.dta, clear

tab t_event sex if year>2014 & howlng!=.
tab t_event sex if year<2015 & howlng!=.

* post variable
gen byr = year*event
ereplace byr = max(byr), by(pidp)

gen post_sp = (byr>2014)

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 5

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate model interacting t_event and reform 
reg howlng ib`base'.t_event ib`base'.t_event#i.post_sp ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 
margins t_event#post_sp
marginsplot 

* marginal effects of t_event by gender
margins post_sp, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local m = 1
	local f = 1 + 1
	
	matrix e_pre  = ( M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_post = ( M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local m = `xx'
	local f = `xx' + 1
	
	matrix e_pre   = (e_pre  \ M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_post  = (e_post \ M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	
}

matrix M = (e_pre , e_post )

svmat M	
* Columns: "Male" "Male, lb"  "Male, ub"  "Female"  "Female, lb"  "Female, ub"
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
colorpalette lin fruits, global opacity(50)
twoway (connected M1 aux, lcolor(${Tangerine}) lpattern(dash_dot) mcolor(${Tangerine}) msymbol("sh")) ///
	   (rcap  M2 M3 aux ,  sort lcolor(${Tangerine})) ///
	   (connected M4  aux   , sort lcolor(${Grape}) msymbol("s") lpattern(dash)  mcolor(${Grape})) ///
	   (rcap      M5 M6 aux ,  sort lcolor(${Grape})), /// 
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Hours of housework relative to t=-1", size(medsmall)) ylabel(#5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) ///
	   graphregion(fcolor(white)) ///
	   legend(order(1 "Pre-reform" 3 "Post-reform" ) row(1) pos(north) size(medsmall)) 
graph export "${graphs}/fertility_hw_d.png", replace



*-------------------------------------------------------------
* 1.5. Shared parental leave  : gendered impact
*-------------------------------------------------------------

use newparent_sample.dta, clear

tab t_event sex if year>2014 & howlng!=.
tab t_event sex if year<2015 & howlng!=.

* post variable
gen byr = year*event
ereplace byr = max(byr), by(pidp)

gen post_sp = (byr>2014)

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 5

* 
*gen gs = 1 if post_sp==0 & sex==0
*replace gs = 2 if post_sp==0 & sex==1
*replace gs = 3 if post_sp==1 & sex==0
*replace gs = 4 if post_sp==1 & sex==1
*
*reg rq ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 
*predict residuals, residuals
*binscatter residuals t_event, by(gs) discrete line(connect) xline(-1)
*
*recode assort (3=2)
*binscatter residuals t_event if sex==1, by(assort) discrete line(connect) xline(-1)


* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'


levelsof post_sp , local(spp)
foreach qt  of local spp {
	* estimate model interacting t_event and sex, per reform status  
	reg howlng ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno if post_sp==`qt', vce(cluster cidp) 

	* marginal effects of t_event by gender
	margins sex, dydx(t_event)

	* store effects in matrix
	matrix list r(table)
	matrix M`qt' = r(table)

	qui sum t_event	
		local m = 1
		local f = 1 + 1
		
		matrix e_pre  = ( M`qt'[1,`m'] , M`qt'[5,`m'] , M`qt'[6,`m'] )	
		matrix e_post = ( M`qt'[1,`f'] , M`qt'[5,`f'] , M`qt'[6,`f'] )	

		local max = `r(max)'*2 - 1
		
	forval xx=3(2)`max' {
		local m = `xx'
		local f = `xx' + 1
		
		matrix e_pre   = (e_pre  \ M`qt'[1,`m'] , M`qt'[5,`m'] , M`qt'[6,`m'] )	
		matrix e_post  = (e_post \ M`qt'[1,`f'] , M`qt'[5,`f'] , M`qt'[6,`f'] )	
	}

	matrix M`qt' = (e_pre , e_post )

	svmat M`qt'

}

egen aux = seq() if M11!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M02 M03 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M12 M13 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M01 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M11 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Housework Hours", size(medsmall)) ylabel(-2(2)8, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Not eligible" 4 "Eligible") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility_hw_e_male.png", replace

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M05 M06 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M15 M16 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M04 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M14 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on Housework Hours", size(medsmall)) ylabel(-2(2)8, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Not eligible" 4 "Eligible") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility_hw_e_female.png", replace



*=======================================================================
* IMPACT ON RQ
*=======================================================================

use newparent_sample.dta, clear

** drop events lags/leads with few obs
*drop if t_event < -4 | t_event > 10
*
*reg rq ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 
*predict residuals, residuals
*binscatter howlng t_event if sex==1, by(dadlv) discrete line(connect) xline(-1)
*
*
*tab t_event sex if rq!=. & parentlv==0
*tab t_event sex if rq!=. & parentlv==1

* post variable
gen byr = year*event
ereplace byr = max(byr), by(pidp)

gen post_sp = (byr>2014)

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 5

* 
gen gs = 1 if post_sp==0 & sex==0
replace gs = 2 if post_sp==0 & sex==1
replace gs = 3 if post_sp==1 & sex==0
replace gs = 4 if post_sp==1 & sex==1

reg rq ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 
predict residuals, residuals
binscatter residuals t_event, by(ggroup) discrete line(connect) xline(-1)

recode assort (3=2)
binscatter residuals t_event if sex==1, by(assort) discrete line(connect) xline(-1)


*------------------------------------------------------------------------
* 1. event study
*------------------------------------------------------------------------
use newparent_sample.dta, clear

tab t_event sex if year>2014 & rq!=.
tab t_event sex if year<2015 & rq!=.

* post variable
gen byr = year*event
ereplace byr = max(byr), by(pidp)

gen post_sp = (byr>2014)

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 5

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

* estimate model interacting t_event and reform 
reg rq ib`base'.t_event ib`base'.t_event#i.post_sp ${indiv_c} ${couple_c}  ib2.wno, vce(cluster cidp) 

* marginal effects of t_event by gender
margins post_sp, dydx(t_event)

* store effects in matrix
matrix list r(table)
matrix M0 = r(table)

qui sum t_event	
	local m = 1
	local f = 1 + 1
	
	matrix e_pre  = ( M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_post = ( M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	

	local max = `r(max)'*2 - 1
	
forval xx=3(2)`max' {
	local m = `xx'
	local f = `xx' + 1
	
	matrix e_pre   = (e_pre  \ M0[1,`m'] , M0[5,`m'] , M0[6,`m'] )	
	matrix e_post  = (e_post \ M0[1,`f'] , M0[5,`f'] , M0[6,`f'] )	
}

matrix M = (e_pre , e_post )

svmat M	
* Columns: "Male" "Male, lb"  "Male, ub"  "Female"  "Female, lb"  "Female, ub"
 
egen aux = seq() if M1!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea M2 M3 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea  M5 M6 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M4 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Not eligible" 4 "Eligible") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility_spp.png", replace


*------------------------------------------------------------------------
* 2. event study by sex
*------------------------------------------------------------------------
use newparent_sample.dta, clear

tab t_event sex if year>2014 & rq!=.
tab t_event sex if year<2015 & rq!=.

* post variable
* post variable
gen byr = year*event
ereplace byr = max(byr), by(pidp)

gen post_sp = (byr>2014)

* drop events lags/leads with few obs
drop if t_event < -4 | t_event > 5

* recode factor variable		--> 	event is t_event==5
sum t_event
local min = -`r(min)' + 1
local base = `min' - 1
replace t_event = t_event + `min'

levelsof post_sp, local(spp)
foreach xx of local spp {
	
	* estimate model interacting t_event and reform 
	reg rq ib`base'.t_event ib`base'.t_event#i.sex ${indiv_c} ${couple_c}  ib2.wno if post_sp==`xx', vce(cluster cidp) 

	* marginal effects of t_event by gender
	margins sex, dydx(t_event)

	* store effects in matrix
	matrix list r(table)
	matrix M`xx' = r(table)

	qui sum t_event	
		local m = 1
		local f = 1 + 1
		
		matrix e_pre  = ( M`xx'[1,`m'] , M`xx'[5,`m'] , M`xx'[6,`m'] )	
		matrix e_post = ( M`xx'[1,`f'] , M`xx'[5,`f'] , M`xx'[6,`f'] )	

		local max = `r(max)'*2 - 1
		
	forval ss=3(2)`max' {
		local m = `ss'
		local f = `ss' + 1
		
		matrix e_pre   = (e_pre  \ M`xx'[1,`m'] , M`xx'[5,`m'] , M`xx'[6,`m'] )	
		matrix e_post  = (e_post \ M`xx'[1,`f'] , M`xx'[5,`f'] , M`xx'[6,`f'] )	
	}

	matrix M`xx' = (e_pre , e_post )

	svmat M`xx'	

	
}

 
egen aux = seq() if M11!=.
replace aux = aux - `min'

sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M02 M03 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M12 M13 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M01 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M11 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Not eligible" 4 "Eligible") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility_spp_men.png", replace


sum aux
local lb = `r(min)'
local ub = `r(max)'
twoway (rarea  M05 M06 aux,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
	   (rarea M15 M16 aux,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
	   (connected M04 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol("sh")) ///
	   (connected M14 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol("sh")), ///
	   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
	   ytitle("Impact on RQ", size(medsmall)) ylabel(-1(.5).5, labsize(small)) ///
	   xtitle("Years around first child birth", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small))   ///
	   legend(order(3 "Not eligible" 4 "Eligible") r(1) pos(12) size(medsmall)) 
graph display, ysize(5) xsize(5)
graph export "${graphs}/fertility_spp_women.png", replace



