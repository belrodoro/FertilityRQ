/*****************************************************************
Project: 	Children and Relationship Quality
Authors: 	Belén Rodríguez Moro and Olatz Román Blanco

Last modified:	2023-02-25
******************************************************************

This file builds the main dataset from Understanding Society

	Output:
	main_full_data.dta : Full individual level BHPS+UKHLS sample, w/ info on RQ and marital histories

	
                               MAIN FILE

	
****************************************************************************/


clear all 
set more off 

if "`c(username)'"=="belen" {
	do "C:/Users/`c(username)'/OneDrive/Documentos/GitHub/FertilityRQ/03_DoFiles/00_globals.do"

}   
if "`c(username)'"=="Olatz" {
	do "C:/Users/`c(username)'/OneDrive - Istituto Universitario Europeo/01_Research/FertilityRQ/03_DoFiles/00_globals.do"
}






*===================================================================================*
*                         BUILD SAMPLES
*===================================================================================*

cd "${path}"


 quietly do "${dofiles}/01_clean_raw_data.do"                // raw_data.dta 
 
 quietly do "${dofiles}/02_build_individual_variables.do"    //   individual_data.dta 
 
 quietly do "${dofiles}/03_build_couple_variables.do"    //   individual_data.dta 
 
 quietly do "${dofiles}/04_construct_data.do"    //   newparent_sanple.dta newcouples_sample.dta 