
use child_panel.dta, clear
keep if ch_num == 1
keep pidp ch_pidp ch_birthy ch_sex

merge 1:n pidp using "main_full_data.dta", keep(2 3)
//     Not matched                       621,920
//     Matched                           435,253

tab newparent _merge
//  newparent | Using      Matched   
//          0 |   619,226    325,735 
//          1 |     2,694    109,518 

twoway (hist ch_birthy if ch_birthy>1990, freq discrete) (hist ch_birthy if ch_birthy>1990 & newparent==1, freq discrete color(${Cherry}))
tab ch_birthy if event==0

tab year if event==0

* drop always old
egen aux = 
