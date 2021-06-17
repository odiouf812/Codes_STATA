* indicators from MICS women file file
/* MICS Dataset 2015 for Mauritania*/

//Heard of female circumcision
gen fg_heard = FG1 
replace fg_heard =1 if FG1==1
label values fg_heard yesno
label var fg_heard "Heard of female circumcision"

/*Circumcised women*/
gen fg_fcircum_wm = FG3==1
replace fg_fcircum_wm=. if FG1==.
label values fg_fcircum_wm yesno
label var fg_fcircum_wm	"Circumcised among women age 15-49"

/* Prevalence of FGM */

tab fg_fcircum_wm [iweight = wmweight]

//Prevalence of FGM by sociodemographic caracteristics

/*wealth index*/

tab  windex5 fg_fcircum_wm [iweight = wmweight], row nofreq

/* level of education*/
tab welevel fg_fcircum_wm [iweight = wmweight], row nofreq

/* Place of residence*/
tab HH6 fg_fcircum_wm [iweight = wmweight], row nofreq

/* Subnational*/
tab HH7 fg_fcircum_wm [iweight = wmweight], row nofreq


//Age at FGM: Age, 25%, 50% and 75%

summarize FG7 [aweight = wmweight] if FG7 <98, detail
