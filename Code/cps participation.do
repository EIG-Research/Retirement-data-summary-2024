* Sarah Eckhardt
* Last Updated: 05/20/2024
* Project: Transfer Income
* Description: compute retirement toplines, as reported
	* by the CPS ASEC supplement
	
cd ""

use cps_00022.dta, clear // CPS data 2014-2023


* Universe of persons:
	* 18-65
	* employed in the reference period
	* non-zero earnings
	* working full time (35 hrs+)
	* not in government work. 
	* employed in non-government position. not self employed.	

	keep if uhrswork1 >=35 & uhrswork1 <999 /* >999 not in universe
	keep if age >=18 & age<65
	keep if classwkr <24 & classwkr >=20
	keep if inctot > 0

/*
* universe for full time persons
	keep if uhrswork1 <999 /* >999 not in universe
	keep if age >=18 & age <=65
	keep if classwkr <24 & classwkr >=20
	keep if inctot > 0
*/
 
/*
	* RSAA universe
	keep if uhrswork1 <999
	keep if age >=16
	keep if classwkr <24 & classwkr >=20
	keep if inctot > 0
	keep if inctot <42200
*/ 


* total labor force w/ specifications: 
 collapse (sum) asecwth, by(year)


/*
gen access = pension == 2 | pension ==3 // retirement plan exists
gen participate = pension ==3 			// included in retirement plan


	* share who have access
/*
collapse (count) obs = pernum [pw=asecwth], by(year access)
bysort year: egen total_obs = sum(obs)
gen percent = (obs / total_obs)*100
*/


	* share who participate
/*
collapse (count) obs = pernum [pw=asecwth], by(year participate)
bysort year: egen total_obs = sum(obs)
gen percent = (obs / total_obs)*100
*/
