*  Sarah Eckhardt
* Last Updated: 05/20/2024
* Project: Transfer Income
* Description: compute retirement toplines, as reported
	* by the Survey of Consumer Finances
	
clear all

* enter user path here:
cd ""

set maxvar 10000
use  p22i6.dta, clear

rename x14 age
rename x42001 wgt
gen full_time = 1*(x4511==1)
gen part_time = 1*(x4511==2)
gen non_government = 1*(x7402 < 9370)

* full time and part time

* universe: 18-65, working.
keep if age >= 18 & age <= 65 & non_government==1 & full_time==1


* participates in retirement plan -
	* (see https://www.federalreserve.gov/econres/files/bulletin.macro.txt,
	* DCPLANCJ
	
gen receives_benefits = (x11032>0|x11132>0|x11332>0|x11432>0| x11032==-1|x11132==-1|x11332==-1|x11432==-1|(x5316==1 & x6461==1) | (x5324==1 & x6466==1)|(x5332==1 & x6471==1) | (x5416==1 & x6476==1))

* has access to retirement plan
gen plan_available = receives_benefits
recode plan_available 0=1 if x4136 == 1


* employer contributions
gen contributions = (x11047==1 | x11147==1)
*  | x11347==1 | x11447==1)




************************************************************
* access, participation, matching as reported in the table *
************************************************************

* access

	collapse(count) obs=y1 [pweight = wgt], by(plan_available)
	drop if plan_available==.
	egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/


* participation
/*
	collapse(count) obs=y1 [pweight = wgt], by(receives_benefits)
	drop if receives_benefits==.
	egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/


* matching
/*
collapse(count) obs=y1 [pweight = wgt], by(contributions)
egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/



**************************
* additional tabulations *
**************************

* participation given access

/*
keep if plan_available==1
	collapse(count) obs=y1 [pweight = wgt], by(receives_benefits)
	drop if receives_benefits==.
	egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/
	

* contributions (has access)
/*
keep if plan_available==1
	collapse(count) obs=y1 [pweight = wgt], by(contributions)
	egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/
/*

* contributions (participating with matching)

keep if receives_benefits ==1
	collapse(count) obs=y1 [pweight = wgt], by(contributions)
	egen total_obs = sum(obs)
	gen percent = obs/total_obs*100
	tab percent
*/
