*----------------------------------------------------------------------------
* (1) Define paths
*----------------------------------------------------------------------------

if "`c(username)'"=="belen" {
    global base "C:/Users/belen/OneDrive - Istituto Universitario Europeo/FertilityRQ"	
	global path "${base}/02_Data/Understanding_Society/stata/stata13_se"
}   
if "`c(username)'"=="Olatz" {
	global base "C:/Users/Olatz/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ"
	global path "C:/Users/Olatz/OneDrive - Istituto Universitario Europeo/02_Datasets/Understanding_Society/stata/stata13_se"
}

	global samp 	"${base}/02_Data"
	global dofiles 	"${base}/03_DoFiles"
	global tabs 	"${base}/04_Output/Tables"
	global graphs 	"${base}/04_Output/Graphs"


*----------------------------------------------------------------------------
* (2) Raw data construction
*----------------------------------------------------------------------------

* All waves 
global BHPSwaves  a b c d e f g h i j k l m n o p q r
global UKHLSwaves a b c d e f g h i j k l 

* Specific variable waves 
global rq_w    a c e g i k						// rq items
global rq2_w   a e g i k						// rq items : engage in outside interests
			
global gnorm_w b d j 							// gender norm items
 
global gbeh_w  b d f h j l						// gender behavior items 
global gbeh2_w a b d f h j l					// gender behavior items : time on hh chores
global gbeh2_bhps_w b c d e f g h i j k l m n o p q r	// gender behavior items (BHPS) : time on hh chores
			
global spl_w   b c d e f g h i j k l			// parental leave

* Control variable groups
global all_c	"dvage birthy sex racel qfhigh_dv employ jbterm1 jbsemp paynu_dv fimnlabgrs_dv fimngrs_dv fimnnet_dv ndepchl_dv hhtype_dv scghql jbhrs scghq1_dv country urban_dv istrtdaty"

global rq_c    "screlpar* screlhappy scdas*"
global rq2_c    "scparoutint"

global gnorm_c  "scopfam*"
global gbeh_c   "hu*"
global gbeh2_c  "howlng"

global spl_c    "matl*"

* RQ measure
global items 	"screlpards screlparrg screlparar screlparir screlparwt screlparei screlparcd screlparks scparoutint screlhappy"	// all items in RQ questionnaire 

global items_time "screlparwt screlparei screlparcd scparoutint screlparks" 
global items_subj "screlpards screlparrg screlparar screlparir screlhappy"


*----------------------------------------------------------------------------
* (3) Analysis - variable groups 
*----------------------------------------------------------------------------

* Unit identifiers 
global unit 	"pidp panel wave"				// unique identifier of each observation
global effects 	"age tenure"					// for backcasting

* Regression controls
global indiv_c   "ib25.age i.sex i.tertiary i.employ c.lincome i.urban_dv"
global couple_c  "ib1.tenure i.married"
global partner_c "ib1.par_agebin i.par_employ i.par_tertiary par_lincome"

* Gender domestic behavior
global gbeh "huboss hubuys hufrys huiron humops" // i removed hudiy hupots to have more data points 


*----------------------------------------------------------------------------
* (4) Plot aesthetics
*----------------------------------------------------------------------------

*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
*ssc install palettes, replace
*ssc install colrspace, replace
*
*set scheme cleanplots
*graph set window fontface "Crimson Text"
*graph set ps fontface "Crimson Text"
*
*graph set window fontface "Times New Roman"

colorpalette lin fruits, global opacity(50)

// ssc install unique
// ssc install egenmore


*----------------------------------------------------------------------------
* (5) Functions
*----------------------------------------------------------------------------
// event study
capture program drop event_study

program define event_study
args outcome event_var event_t indep_vars clust_var y_labels
	
	* 0. variable name
	local varname : variable label `outcome' 

	* 1. recode factor variable
	qui sum `event_var'
		local min = -`r(min)' + 1
		local base = `min' - `event_t'
	replace `event_var' = `event_var' + `min'

	* 2. estimate model 
	reg `outcome' ib`base'.`event_var' `indep_vars', vce(cluster `clust_var') 

	tempfile event_study
	parmest, saving(`event_study', replace)

	* 3. modify stored estimates 
	preserve
	use `event_study', clear

	egen event = seq() if strpos(parm, "`event_var'") > 0
	drop if event == .
	replace event = event - `min'

	rename min95 ci_lb
	rename max95 ci_ub
	gen coeff = round(estimate, 0.001)

	* 4. plot 
		
	* x-axis labels
	qui sum event
		local lb = `r(min)'
		local ub = `r(max)'
			
	colorpalette lin fruits, locals
	twoway (rarea  ci_lb ci_ub event ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
		   (connected coeff event, lcolor(`Tangerine') lpattern(dash_dot) mcolor(`Tangerine') msymbol("sh")) , ///
		   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("Impact on `varname'", size(medsmall)) ylabel(`y_labels', labsize(small)) yscale(outergap(*-3) titlegap(*-3))  ///
		   xtitle("Event-time (years)", size(medsmall)) xlabel(`lb'(2)`ub', labsize(small)) yscale(outergap(*-3))  ///
		   legend(off) 
		   
	restore
	replace `event_var' = `event_var' - `min'
end

// event study: marginal effects 
capture program drop event_margins
program define event_margins
args outcome event_var event_t heterog_var indep_vars clust_var y_labels
	
	* 0. variable name
	local varname : variable label `outcome' 
	local legname : value label `heterog_var'

	* 1. recode factor variable
	qui sum `event_var'
		local min = -`r(min)' + 1
		local base = `min' - `event_t'
	replace `event_var' = `event_var' + `min'

	* 2. estimate model 
	reg `outcome' ib`base'.`event_var' ib`base'.`event_var'#i.`heterog_var' `indep_vars', vce(cluster `clust_var') 

	* 3. marginal effects of t_event
	margins `heterog_var', dydx(`event_var')

	* 4. store effects in matrix
	matrix list r(table)
	matrix M0 = r(table)
	
	* intitiate per group matrices 
	qui sum t_event	
		local g1 = 1
		local g2 = 1 + 1
		
		matrix e_g1 = ( M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
		matrix e_g2 = ( M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	

		local max = `r(max)'*2 - 1
		
	* remaining periods	
	forval xx=3(2)`max' {
		local g1 = `xx'
		local g2 = `xx' + 1
		
		matrix e_g1   = (e_g1 \ M0[1,`g1'] , M0[5,`g1'] , M0[6,`g1'] )	
		matrix e_g2   = (e_g2 \ M0[1,`g2'] , M0[5,`g2'] , M0[6,`g2'] )	
	}

	matrix M = (e_g1 , e_g2 )

	* turn into variables 
	svmat M	
	
	* 5. plot 
	
	* x-axis labels
	egen aux = seq() if M1!=.
	replace aux = aux - `min'
	sum aux
		local lb = `r(min)'
		local ub = `r(max)'
		
	* legend 
	qui levelsof `heterog_var', local(levels)
	local x = 1
	foreach z of local levels {
		local leg`x' : label `legname' `z'
		local x = `x' + 1
	}	

	twoway (rarea  M2 M3 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
		   (rarea  M5 M6 aux ,  sort color(${Tangerine}) fint(inten30) lw(none)) ///
		   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol(smsquare_hollow)) ///
		   (connected M4 aux, lcolor(${Tangerine}) lpattern(dash) mcolor(${Tangerine}) msymbol(smsquare_hollow)), ///
		   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("Impact on `varname'", size(medsmall)) ylabel(`y_labels', labsize(small)) yscale(outergap(*-3) titlegap(*-3))  ///
		   xtitle("Event time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) xscale(outergap(*-3))  ///
		   legend(order(3 "`leg1'" 4 "`leg2'") row(1) pos(12) size(medsmall)) 
end

// event study: significant differences 
capture program drop event_difference
program define event_difference
args outcome event_var event_t heterog_var indep_vars clust_var y_labels
	
	* 1. recode factor variable
	qui sum `event_var'
		local min = -`r(min)' + 1
		local base = `min' - `event_t'
	replace `event_var' = `event_var' + `min'

	* 2. estimate fully saturated model 
	reg `outcome' (ib`base'.`event_var' `indep_vars')##i.`heterog_var', vce(cluster `clust_var') 

	* 3. store results in a matrix 
	matrix M = (_b[1.`event_var'#1.`heterog_var'], ///
				_b[1.`event_var'#1.`heterog_var'] - invttail(e(df_r),0.025)*_se[1.`event_var'#1.`heterog_var'] , ///
				_b[1.`event_var'#1.`heterog_var'] + invttail(e(df_r),0.025)*_se[1.`event_var'#1.`heterog_var'] )	
	qui sum `event_var'	
	forval xx=2/`r(max)' {
		matrix M = (M \ _b[`xx'.`event_var'#1.`heterog_var'], ///
					_b[`xx'.`event_var'#1.`heterog_var'] - invttail(e(df_r),0.025)*_se[`xx'.`event_var'#1.`heterog_var'] , ///
					_b[`xx'.`event_var'#1.`heterog_var'] + invttail(e(df_r),0.025)*_se[`xx'.`event_var'#1.`heterog_var'] )
	}

	* turn into variables 
	svmat M	
	
	* 5. plot 
	
	* x-axis labels
	egen aux = seq() if M1!=.
	replace aux = aux - `min'
	qui sum aux
		local lb = `r(min)'
		local ub = `r(max)'

	twoway (rarea  M2 M3 aux ,  sort color(${Blueberry}) fint(inten30) lw(none)) ///
		   (connected M1 aux, lcolor(${Blueberry}) lpattern(dash) mcolor(${Blueberry}) msymbol(smsquare_hollow)), ///
		   xline(-.5, lpattern(dash) lcolor(gs11)) yline(0) ///
		   ytitle("Additional Impact on Mothers", size(medsmall)) ylabel(`y_labels', labsize(small)) yscale(outergap(*-3) titlegap(*-3))  ///
		   xtitle("Event time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) xscale(outergap(*-3))  ///
		   legend(off) 
end






//
//
// capture program drop event_margins
// program define event_margins
// args outcome event_var heterog_var indep_vars clust_var
//	
// 	* 0. variable name
// 	local varname : variable label `outcome' 
//
// 	* 1. recode factor variable
// 	qui sum `event_var'
// 		local min = -`r(min)' + 1
// 		local base = `min' - 1
// 	replace `event_var' = `event_var' + `min'
//
// 	* 2. estimate model 
// 	reg `outcome' ib`base'.`event_var' ib`base'.`event_var'#i.`heterog_var' `indep_vars', vce(cluster `clust_var') 
//
// 	* 3. marginal effects of t_event
// 	margins `heterog_var', dydx(`event_var')
//
// 	* 4. store effects in matrix
// 	matrix list r(table)
// 	matrix M0 = r(table)
//	
// 	* intitiate per group matrices 
// 	qui unique `heterog_var'
// 		local gN = `r(unique)'
// 	forval z = 1 (1) `gN' {
// 		matrix e_g`z'   = ( M0[1,`z'] , M0[5,`z'] , M0[6,`z'] )			
// 	}
//	
// 	* rest of the values 
// 	qui sum `event_var'
// 		local max = `r(max)'
// 	forval z = 1 (1) `gN' {
// 	forval xx = 2 (1) `max' {
// 		local pos = `gN'*(`xx'-1) + `z'
// 		matrix e_g`z'   = ( e_g`z' \ M0[1,`pos'] , M0[5,`pos'] , M0[6,`pos'] )							
// 	}
// 	}
//
// 	* store in single matrix
// 	matrix M = e_g1 
// 	forval z = 2 (1) `gN' {
// 		matrix M = (M , e_g`z')		
// 	}
//
// 	* turn into variables 
// 	svmat M	
//	
// 	* 5. plot 
//	
// 	* color list  
// 	local color_list `" "Blueberry" "Tangerine" "Grape" "Apple" "Cherry" "Peach" "Banana" "'
//	
// 	* x-axis labels
// 	egen aux = seq() if M1!=.
// 	replace aux = aux - `min'
// 	sum aux
// 		local lb = `r(min)'
// 		local ub = `r(max)'
//	
// 	* initialize graph 
// 	local ci_plot ""
// 	local  b_plot ""
//	
// 	* loop over groups  
// 	forval z = 1 (1) `gN' {
//		
// 		* locals: coefficient, ub and lb 
// 		local coef = 1 + (`z' - 1) * 3 
// 		local lbc  = `coef' + 1
// 		local ubc  = `coef' + 2 
//		
// 		* y-axis labels
// 		qui sum M`lbc' 
// 			local min = floor(`r(min)')
// 		qui sum M`ubc' 
// 			local max = ceil(`r(max)')
//			
// 		* color 
// 		local colnum : word `z' of `color_list'
//
// 		* store plot 
// 		local ci_plot = "`ci_plot' (rarea M`lbc' M`ubc' aux , sort color(${\`colnum'\}) fint(inten30) lw(none)) "
// 		local  b_plot =  "`b_plot' (connected M`coef' aux, lcolor(${\`colnum'\}) lpattern(dash) mcolor(${\`colnum'\}) msymbol(smsquare_hollow)) "
//	
// 	}	
//
// 	twoway `ci_plot' `b_plot', ///
// 		   xline(-1, lpattern(dash) lcolor(gs11)) yline(0) ///
// 		   ytitle("Impact on `varname'", size(medsmall)) ylabel(#5, labsize(small)) ///
// 		   xtitle("Event time (years)", size(medsmall)) xlabel(`lb'(1)`ub', labsize(small)) ///
// 		   legend(row(1) pos(12) size(medsmall)) 
// end
//
//
// event_margins rq t_event sex "${indiv_c} ${couple_c}" cidp