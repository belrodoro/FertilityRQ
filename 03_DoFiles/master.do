clear all 
set more off 

if "`c(username)'"=="belen" {
	global dofiles  "C:\Users\belen\OneDrive - Istituto Universitario Europeo\FertilityRQ\03_DoFiles"
}   
if "`c(username)'"=="Olatz" {
	global dofiles "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles"
}



*===============================
* (0) Define globals and presets
*===============================

	do "${dofiles}/00_globals.do"


*===================================================================
* (1) Create full individual- and couple-level panels using raw data
*===================================================================

	do "${dofiles}/01_build_panel.do"


*=========================================
* (2) Create individual and couple samples
*=========================================

	do "${dofiles}/02_build_samples.do"


*=========================================
* (3) Descriptives
*=========================================

	do "${dofiles}/03_descriptives.do"


*=========================================
* (4) Main analysis
*=========================================

	do "${dofiles}/04_analysis.do"


*=========================================
* (5) Mechanisms
* impact of birth on different outcomes
*=========================================

	do "${dofiles}/05_mechanisms.do"

	
*=========================================
* (6) Heterogeneity
* impact on RQ by categories of diff outcomes 
*=========================================

	do "${dofiles}/06_heterogeneity.do"


*=========================================
* (7) Parental leave
*=========================================

	do "${dofiles}/07_parental_leave.do"


*=========================================
* (8) Robustness
*=========================================

	do "${dofiles}/08_robustness.do"
