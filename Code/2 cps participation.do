* Sarah Eckhardt
* Last Updated: 09/19/2024
* Project: Transfer Income
* Description: compute retirement toplines, as reported
	* by the CPS ASEC supplement

* enter user data path here -- 
cd ""

use cps_data.dta, clear // CPS data download from IPUMS, 2023


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
* universe for full AND part time person count
	keep if uhrswork1 <999 /* >999 not in universe
	keep if age >=18 & age <=65
	keep if classwkr <24 & classwkr >=20
	keep if inctot > 0
*/
 
/*
	* RSAA universe: for RSAA population calculations
	keep if uhrswork1 <999
	keep if age >=16
	keep if classwkr <24 & classwkr >=20
	keep if inctot > 0
	keep if inctot <42200 /* comment out for full universe. this is eligibility
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
