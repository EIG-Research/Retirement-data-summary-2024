* Sarah Eckhardt
* Last Updated: 05/20/2024
* Project: Transfer Income
* Description: compute retirement toplines, as reported
	* by the CPS ASEC supplement
	
cd "/Users/sarah/Library/CloudStorage/GoogleDrive-sarah@eig.org/My Drive/projects/retirement/data/CPS"

use cps_00013.dta, clear // CPS data 2014-2023

* Universe of persons:
	* 18-65
	* employed in the reference period
	* working full time.
	* not in government work. 

keep if age >=18 & age <=65 & (empstat==10 | empstat==12)

gen full_time = 1*(uhrswork1 >=35) + 2*(uhrswork1 >0 & uhrswork1 <35)

keep if full_time ==1

* non-government work
keep if ind < 9370

/*
* total labor force 
collapse (count) obs = pernum [pw=asecwth], by(year)
*/

gen access = pension == 2 | pension ==3 // retirement plan exists
gen participate = pension ==3 			// included in retirement plan

	* share who have access
/*
collapse (count) obs = pernum [pw=asecwth], by(year access)
bysort year: egen total_obs = sum(obs)
gen percent = (obs / total_obs)*100
*/
/*
	* share who participate

collapse (count) obs = pernum [pw=asecwth], by(year participate)
bysort year: egen total_obs = sum(obs)
gen percent = (obs / total_obs)*100
*/

