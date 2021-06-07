********************************************************************************
********************************************************************************
/*
Does the Common Core Have a Common Effect? An Exploration of Effects on Academically Vulnerable Students

by Joshua Bleiberg
AERA Open 2021
https://doi.org/10.1177/23328584211010727

Paper published April 26, 2021 and replication code posted June 7, 2021

Analysis requires access to NAEP Restricted Use Data:
“Student and Teacher Survey,” 2002–2003, 2004–2005, 2006–2007, 2008–2009, 2010–2011, 2012–2013
*/
********************************************************************************
********************************************************************************
cls
clear all
set max_memory 1600g
set niceness 1
********************************************************************************
********************************************************************************
//Creating Log File
//Direction: Set global path to be the folder that includes sub-folders "data" and "output"
global path "!!!!!!!!!!!!!!!!!!!!!!!!!SET PATH!!!!!!!!!!!!!!!!!!!!!!!!!"
log close _all
log using "${path}output\ccss_log.smcl", replace
********************************************************************************
//Create NAEP Panel from Raw Files
********************************************************************************
********************************************************************************
//Create Master NAEP Files from MG4, MG8, RG4, RG8 panels. Code to create grade/subject panels available upon request.
use "${path}data\naep_math_8", clear
append using "${path}data\naep_math_4" "${path}data\naep_read_4" "${path}data\naep_read_8"
//Gender
rename dsex sex
recode sex (1=0)(2=1)
label define sex_labels 0 "Male" 1 "Female"
label values sex sex_labels
//IEP
recode iep (2=0) (8=.)
label define iep_labels 0 "No IEP" 1 "Yes IEP"
label values iep iep_labels
//LEP
recode lep (2=0) (8=.)
label define lep_labels 0 "No LEP" 1 "Yes LEP"
label value lep lep_labels
//FRPL
rename slunch1 slunch
recode slunch (3=.) (1=0) (2=1)
label define lunch_labels 0 "Eligible" 1 "Not Eligible"
label value slunch lunch_labels
//Race
rename sdracem race
recode race (1=0) (2=1) (3=2) (4=3) (5=4) (6=5) (8=.)
label define race_labels 0 "White" 1 "Black" 2 "Hispanic" 3 "Asian/Pacif Islander" 4 "American Indian" 5 "Other Race"
labe values race race_labels
//Days absent
rename b018101 day_absent
recode day_absent (0=.) (1=0) (2=1) (3=2) (4=3) (5=4) (8=.)
label define day_absent_label 0 "None" 1 "1-2 days" 2 "3-4 days" 3 "5-10 days" 4 "More than 10 days"
label values day_absent day_absent_label

*Dropping non-US territories and missing a state
drop if inlist(fips,60,97,98,99,.)
********************************************************************************
********************************************************************************
//Creating NCES district ID
gen nces_dist_id=substr(ncessch,1,7)
destring nces_dist_id, force replace
*Fixing Memphis/Shelby
replace nces_dist_id=4700148  if nces_dist_id==473810 | nces_dist_id==4702940 
//Merging non-NAEP covariates
*Fixing NYC IDs
replace nces_dist_id=3620580 if inlist(nces_dist_id, 3600076, 3600077, 3600078 ///
, 3600079, 3600081, 3600083, 3600084, 3600085, 3600086, 3600087 	///
, 3600088, 3600090, 3600091, 3600119, 3600092, 3600094, 3600095 	///
, 3600096, 3600120, 3600151, 3600152, 3600153, 3600121, 3600098 	///
, 3600122, 3600099, 3600123, 3600100, 3600101, 3600102, 3600103 	///
, 3600097) & inlist(year,2011,2013,2015,2017)

********************************************************************************
//Standardizing outcomes within dataset and year
local run 1 2 3 4 5
foreach var of local run{
gen sd_mrpcm`var'=.
gen sd_rrpcm`var'=.
}
local years 2003 2005 2007 2009 2011 2013 2015 2017
foreach y of local years{

local dep_var mrpcm1 mrpcm2 mrpcm3 mrpcm4 mrpcm5
foreach var of local dep_var{


qui sum `var' if data_set==1 & year==`y' & chrtrpt==2 & schtype==1 [aw=origwt]
replace sd_`var'=(`var'-r(mean))/r(sd) if data_set==1 & year==`y'


qui sum `var' if data_set==2 & year==`y' & chrtrpt==2 & schtype==1 [aw=origwt]
replace sd_`var'=(`var'-r(mean))/r(sd) if data_set==2 & year==`y'
}
local dep_var rrpcm1 rrpcm2 rrpcm3 rrpcm4 rrpcm5
foreach var of local dep_var{


qui sum `var' if data_set==3 & year==`y' & chrtrpt==2 & schtype==1 [aw=origwt]
replace sd_`var'=(`var'-r(mean))/r(sd) if data_set==3 & year==`y'


qui sum `var' if data_set==4 & year==`y' & chrtrpt==2 & schtype==1 [aw=origwt]
replace sd_`var'=(`var'-r(mean))/r(sd) if data_set==4 & year==`y'
}
}

*Labeling Variables
label variable sd_mrpcm1 "Standardized Math Value 1"
label variable sd_mrpcm2 "Standardized Math Value 2"
label variable sd_mrpcm3 "Standardized Math Value 3"
label variable sd_mrpcm4 "Standardized Math Value 4"
label variable sd_mrpcm5 "Standardized Math Value 5"

label variable sd_rrpcm1 "Standardized Reading Value 1"
label variable sd_rrpcm2 "Standardized Reading Value 2"
label variable sd_rrpcm3 "Standardized Reading Value 3"
label variable sd_rrpcm4 "Standardized Reading Value 4"
label variable sd_rrpcm5 "Standardized Reading Value 5"

label variable data_set "Data Set"
label variable chrtrpt "Charter School"

*Generating Pooled Outcomes
local numb 1 2 3 4 5
foreach i of local numb{
gen pv_`i'=sd_mrpcm`i' if inlist(data_set,1,2)
replace pv_`i'=sd_rrpcm`i' if inlist(data_set,3,4)
}

label variable pv_1 "Plausible Value 1"
label variable pv_2 "Plausible Value 2"
label variable pv_3 "Plausible Value 3"
label variable pv_4 "Plausible Value 4"
label variable pv_5 "Plausible Value 5"

*Creating student race/ethnicity
qui tab race, gen(race_)
label variable race_1 "White"
label variable race_2 "Black"
label variable race_3 "Hispanic"
label variable race_4 "Asian/Pacif Islander"
label variable race_5 "American Indian"
label variable race_6 "Other Race"

gen race_detail=race
recode race_detail (2=.) (3=2) (4=3) (5=4)
replace race_detail=5 if bb21101==1
replace race_detail=6 if bc21101==1
replace race_detail=7 if bd21101==1
replace race_detail=8 if be21101==1
label define race_det_labels 0 "White" 1 "Black"  2 "Asian/Pacif Islander" 3 "American Indian" 4 "Other Race" 5 "Mexican" 6 "Puerto Rican" 7 "Cuban" 8 "Other Hispanic"
labe values race_detail race_det_labels
qui tab race_detail, gen(race_detail_)
label variable race_detail_1 "White"
label variable race_detail_2 "Black"
label variable race_detail_3 "Asian/Pacif Islander"
label variable race_detail_4 "American Indian"
label variable race_detail_5 "Other"
label variable race_detail_6 "Mexcian"
label variable race_detail_7 "Puerto Rican"
label variable race_detail_8 "Cuban"
label variable race_detail_9 "Other Hispanic"

********************************************************************************
********************************************************************************
//Cleaning and Merging Data
********************************************************************************
********************************************************************************
*Treatment
/*Common Core Standards
"cc_standards_policies_5_25_2021" contains treatment data. These variables used 
to create treatment have no value for 2015 & 2017 intentionally.
*/
gen str subject="."
replace subject="ELA" if inlist(data_set,3,4)
replace subject="Math" if inlist(data_set,1,2)
merge m:1 fips year subject using "${path}data\cc_standards_policies_5_25_2021.dta", gen(cc_policies_merge)
//Dropping American Territories and DDODEA
drop if cc_policies_merge==1
/*Fixing New Jersey, which is the only state with a grade split correct for math
Can't do this in the orginal file because of the unit of anlaysis.*/
recode cc_all (1=0) if data_set==2 & statename=="New Jersey"
//Creating Ever Common Core
drop cc_ever
bysort fips data_set: egen cc_ever=max(cc_all)
********************************************************************************
********************************************************************************
//Year dummies
qui tab year, gen(year_)
//Creating new other variable
gen race_other=0
replace race_other=1 if race_4==1 | race_5==1 | race_6==1
label variable race_other "Asian, Indian, or Other"
//Generating Heterogenous Effects over time
gen cc_all_2011=cc_all
replace cc_all_2011=0 if year==2013
gen cc_all_2013=cc_all
replace cc_all_2013=0 if year==2011
//Recoding Lunch
recode slunch (0=1) (1=0)
label define lunch_label 1 "Eligible" 0 "Not Eligible"
label value slunch lunch_label
*Merging all covariates by school district
//Dropping extraneous variables
rename ncessch nces_sch
destring nces_sch, force replace
//Master School Covariates from CCD
merge m:1 nces_sch year using "${path}data\master_covariates.dta", gen(master_gen)
keep if inlist(year,2003,2005,2007,2009,2011,2013)

tostring nces_sch, gen(nces_str) format("%012.0f")
drop nces_dist_id
gen nces_dist_id=substr(nces_str,1,7)
destring nces_dist_id, force replace

//Merging in the spending variable
merge m:1 nces_dist_id year using "${path}data\spending.dta", nogen

//Merging AYP Flag http://www.gsb.columbia.edu/nclb
merge m:1 nces_sch using "${path}data\national_2003.dta", nogen

//Title I
recode title_1 (2=0)

//Creating AFT low variable
gen aft_low=0 if inlist(data_set,1,2,3,4)
replace aft_low=1 if aft_low_math_e==1 & data_set==1
replace aft_low=1 if aft_low_math_m==1 & data_set==2
replace aft_low=1 if aft_low_ela_e==1 & data_set==3
replace aft_low=1 if aft_low_ela_m==1 & data_set==4

//Merging with school NAEP data
merge m:1 year  schid data_set using "${path}data\naep_school_all.dta", keep(match)

//Creating event study variables
local years 2003 2005 2007 2009 2011 2013
foreach i of local years{
gen cc_`i'=0
replace cc_`i'=1 if cc_ever==1 & year==`i'
}

//Merging in the school covariates
merge m:1 nces_sch using "${path}data\school_baseline_2003.dta", nogen
********************************************************************************
********************************************************************************
//Merging lagged score
preserve
//Math 2000 grade 4
use "${path}data\stud_tch_2000_math_g4.dta", clear
qui sum mrpcm1 [aw=origwt]
gen pv_1=.
replace pv_1=(mrpcm1-r(mean))/r(sd)
drop year
gen year=2000
gen data_set=1
collapse (mean) pv_1 [pw=origwt], by(fips year data_set)
tempfile math_g4_pre03
save "`math_g4_pre03'"
//Math 2000 grade 8
use "${path}data\stud_tch_2000_math_g8.dta", clear
qui sum mrpcm1 [aw=origwt]
gen pv_1=.
replace pv_1=(mrpcm1-r(mean))/r(sd)
drop year
gen year=2000
gen data_set=2
collapse (mean) pv_1 [pw=origwt], by(fips year data_set)
tempfile math_g8_pre03
save "`math_g8_pre03'"
//Reading 2002 grade 4
use "${path}data\stud_tch_2002_read_g4.dta", clear
qui sum rrpcm1 [aw=origwt]
gen pv_1=.
replace pv_1=(rrpcm1-r(mean))/r(sd)
drop year
gen year=2002
gen data_set=3
drop fips
rename fips02 fips
collapse (mean) pv_1 [pw=origwt], by(fips year data_set)
tempfile read_g4_pre03
save "`read_g4_pre03'"
//Reading 2002 grade 8
use "${path}data\stud_tch_2002_read_g8.dta", clear
qui sum rrpcm1 [aw=origwt]
gen pv_1=.
replace pv_1=(rrpcm1-r(mean))/r(sd)
drop year
gen year=2002
gen data_set=4
drop fips
rename fips02 fips
collapse (mean) pv_1 [pw=origwt], by(fips year data_set)
tempfile read_g8_pre03
save "`read_g8_pre03'"

use "`math_g4_pre03'", clear
append using "`math_g8_pre03'" "`read_g4_pre03'" "`read_g8_pre03'"
save "${path}data\lagged_scores_2003.dta", replace
restore

//Merging Lagged Scores
preserve
collapse (mean) pv_1 [pw=origwt], by(fips year data_set)
append using "${path}data\lagged_scores_2003.dta"
sort data_set fips year
bysort data_set fips: gen pv_lag1=pv_1[_n-1]
bysort data_set fips: gen pv_lag2=pv_1[_n-2]
save "${path}data\lagged_scores.dta", replace
restore

//Merging Baseline 2003
preserve
collapse (mean) pv_1, by(fips year data_set)
keep if year==2003
rename pv_1 pv_1_base03
save "${path}data\baseline_2003.dta", replace
restore

//Merging Baseline 00/02
preserve
use "${path}data\lagged_scores_2003.dta", clear
collapse (mean) pv_1, by(fips year data_set)
keep if inlist(year,2000,2002)
rename pv_1 pv_1_base00_02
save "${path}data\baseline_00_02.dta", replace
restore

merge m:1 fips year data_set using "${path}data\lagged_scores.dta", nogenerate
	label variable pv_lag1 "State Lag 1 Wave"
	label variable pv_lag2 "State Lag 2 Wave"
merge m:1 fips data_set using "${path}data\baseline_2003.dta", nogenerate
	label variable pv_1_base03 "State 2003 NAEP Baseline"
merge m:1 fips data_set using "${path}data\baseline_00_02.dta", nogenerate
	label variable pv_1_base00_02 "State 00/02 NAEP Baseline"
	
********************************************************************************
********************************************************************************
*Labeling Mediators
label variable madeayp "School AYP Status 2003)"
label define madeayp_labels 0 "No" 1 "Yes" 2 "Safe Harbor"
label values madeayp madeayp_labels

//Making Binary Variables
qui tab modage, gen(modal_grade)
qui tab year, gen(year)
qui tab madeayp, gen(madeayp)
********************************************************************************
********************************************************************************
*Analysis
drop area-eduplan
drop if pv_1==.
keep if chrtrpt==2
cls
compress
save "${path}data\cc_data_11_22_2020.dta", replace
********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
//Figure 3. NAEP Score Trends
local d_set 1 2 3 4
foreach i of local d_set{
di "Loop `i'"
qui mean pv_1 if data_set==`i' [pw=origwt], over(cc_ever year)

mat pv=r(table)'
mat pv_con=pv[1..6,1]
mat pv_treat=pv[7..12,1]
mat ll_con=pv[1..6,5]
mat ll_treat=pv[7..12,5]
mat ul_con=pv[1..6,6]
mat ul_treat=pv[7..12,6]

svmat pv_con
svmat pv_treat
svmat ll_con
svmat ll_treat
svmat ul_con
svmat ul_treat

gen year_graph=.
replace year_graph=2003 if _n==1
replace year_graph=2005 if _n==2
replace year_graph=2007 if _n==3
replace year_graph=2009 if _n==4
replace year_graph=2011 if _n==5
replace year_graph=2013 if _n==6

graph twoway rcap ll_con1 ul_con1 year_graph, color(blue*.75) lwidth(thick) || ///
			 line pv_con1 year_graph, lcolor(blue*.75) lwidth(thick) || ///
			 rcap ll_treat1 ul_treat1 year_graph, color(red*.75) lwidth(thick)  || ///	
			 line pv_treat1 year_graph, lcolor(red*.75)  lwidth(thick) ///
	title("", size(small)) ///
	ytitle("NAEP (Standardized)", size(small)) ///
	ylabel(,labsize(vsmall)  labc(black)) ///
	xtitle("Year", size(small)) ///
	xline(2009, lcolor(gs10) lwidth(vthick)) ///
	xlabel(2003(2)2013, labsize(small) labc(black) ) ///
	ylabel(-0.16(.01).06) ///
	legend(off) color(black)
graph save "${path}output\ptrend_all_dset_`i'.gph", replace
graph export "${path}output\ptrend_all_dset_`i'.png", as(png) replace
drop pv_con1 pv_treat1 ll_con1 ll_treat1 ul_con1 ul_treat1 year_graph
}
graph combine "${path}output\ptrend_all_dset_1.gph" ///
              "${path}output\ptrend_all_dset_2.gph" ///
			  "${path}output\ptrend_all_dset_3.gph" ///
              "${path}output\ptrend_all_dset_4.gph", row(2) col(2)
graph export "${path}output\ptrend_all_dset_all.png", as(png) replace
********************************************************************************
********************************************************************************
//Making Analytic Sample Restrictions
keep if major_revision==0  & fordham_low==1
drop if inlist(statename,"Texas","Virginia")
drop if statename=="Minnesota" & data_set==1
drop if statename=="Minnesota" & data_set==2
********************************************************************************
********************************************************************************
//Table 1. Effect of Common Core on NAEP Scores
local d_set 1 2 3 4
foreach i of local d_set{
local regvars i.sex i.iep i.lep i.slunch i.race_detail i.modage i.madeayp pv_lag1
qui reghdfe pv_1 cc_all_2011 cc_all_2013 ///
	`regvars' [pw=origwt] if data_set==`i' , ///
	vce(cluster nces_sch) absorb(nces_dist_id year)
estimate store low_d_fe_wcov_`i'
}
esttab low_d_fe_wcov_1 ///
	   low_d_fe_wcov_2 ///
	   low_d_fe_wcov_3 ///
	   low_d_fe_wcov_4 ///
	using "${path}output\regs_low_sample.csv", ///
	title("Table 1. Effect of Common Core on NAEP Scores") ///
	coeflabel(cc_all_2011 "CC 2011" cc_all_2013 "CC 2013") ///
	label ///
	nodepvars ///
	b(3) ///
	t(3) ///
	ar2 (3) ///
	scalar(F) ///
	sfmt (2 0 0 0) ///
	replace  ///
	se(3) ///
	nomtitle ///
	nogaps ///
	nonote ///
	keep(cc_all_2011 cc_all_2013)
********************************************************************************
********************************************************************************
//Figure 4. Event Study on 4th and 8th Grade Math
local dset 1 2 3 4
foreach i of local dset{
local regvars i.sex i.iep i.lep i.slunch i.race_detail i.modage i.madeayp pv_lag1
qui reghdfe pv_1 cc_2003 cc_2005 cc_2007 cc_2011 cc_2013 ///
	`regvars' if data_set==`i' [pw=origwt], ///
	vce(cluster nces_sch) absorb(nces_dist_id year)	
	estimate store event_dfe_c_`i'
}
local dset 1 2 3 4
foreach i of local dset{

*Make a Blank Matrix
mat event=J(6,2,.)
*Restoring Estimate
estimates restore event_dfe_c_`i'
*Pull beta and SE
mat event[1,1]=_b[cc_2003]
mat event[1,2]=_se[cc_2003]
mat event[2,1]=_b[cc_2005]
mat event[2,2]=_se[cc_2005]
mat event[3,1]=_b[cc_2007]
mat event[3,2]=_se[cc_2007]
mat event[4,1]=0
mat event[4,2]=0
mat event[5,1]=_b[cc_2011]
mat event[5,2]=_se[cc_2011]
mat event[6,1]=_b[cc_2013]
mat event[6,2]=_se[cc_2013]
*Creating Variables from Matrix
svmat event
gen hi_event=event1+1.96*event2
gen low_event_event=event1-1.96*event2

gen model=_n
replace model=. if _n>6
*Graphing
graph twoway  bar event1 model if model==1,  color(ltblue) || rcap hi_event low_event model if model==1, color(black) || ///
	          bar event1 model if model==2,  color(ltblue) || rcap hi_event low_event model if model==2, color(black) || ///
			  bar event1 model if model==3,  color(ltblue) || rcap hi_event low_event model if model==3, color(black) || ///
	          bar event1 model if model==4, msymbol(D) color(ltblue) || rcap hi_event low_event model if model==4, color(black) || ///
	          bar event1 model if model==5,  color(ltblue) || rcap hi_event low_event model if model==5, color(black) || ///
	          bar event1 model if model==6,  color(ltblue) || rcap hi_event low_event model if model==6, color(black) ///
	xlabel(1 "Pre-Treatment 2003" 2 "Pre-Treatment 2005" ///
	3 "Pre-Treatment 2007" 4 "Baseline- 2009" 5 "CC Preparation" 6 "CC Implementation", angle(forty_five) labc(black) labs(vsmall)) ///
	ytitle("NAEP (Standardized)") ylabel(  -0.08(.01).14, labc(black) labs(vsmall)) legend(off) xtitle("")
graph save "${path}output\event_study_d`i'.gph", replace
graph export "${path}output\event_study_d`i'.png", as(png) replace
drop event1 event2 hi_event low_event_event model
}
graph combine "${path}output\event_study_d1.gph" ///
              "${path}output\event_study_d2.gph" ///
			  "${path}output\event_study_d3.gph" ///
              "${path}output\event_study_d4.gph", row(2) col(2)
graph export "${path}output\event_study_d_all.png", as(png) replace
*******************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
//Figure 5. Common Core Effects on Achievement Gaps
local dset 1 2 3 4
foreach i of local dset{
local regvars i.sex i.iep i.lep i.modage i.madeayp pv_lag1
qui reghdfe pv_1 i.cc_all_2011 i.cc_all_2013##i.race i.cc_all_2013#i.slunch i.slunch ///
	`regvars' ///
	[pw=origwt] if data_set==`i', ///
	vce(cluster nces_sch) absorb(nces_dist_id year)
estimate store hetero_dfe_wcov_`i'
}
local dset 1 2 3 4
foreach i of local dset{
estimate restore hetero_dfe_wcov_`i'
*Make a Blank Matrix
mat het_plot_bar=J(9,2,.)

//White and Econ Advantaged
qui lincom (1.cc_all_2013)
	mat het_plot_bar[1,1]=r(estimate)
	mat het_plot_bar[1,2]=r(se)

//Blanks
mat het_plot_bar[2,1]=0
mat het_plot_bar[2,2]=0		
	
//Black
qui lincom (1.cc_all_2013 + 1.cc_all_2013#1.race)
	mat het_plot_bar[3,1]=r(estimate)
	mat het_plot_bar[3,2]=r(se)

//Blanks
mat het_plot_bar[4,1]=0
mat het_plot_bar[4,2]=0		
	
//Hispanic
qui lincom (1.cc_all_2013 + 1.cc_all_2013#2.race)
	mat het_plot_bar[5,1]=r(estimate)
	mat het_plot_bar[5,2]=r(se)

//Blanks
mat het_plot_bar[6,1]=0
mat het_plot_bar[6,2]=0		
	
//Asian
qui lincom (1.cc_all_2013 + 1.cc_all_2013#3.race)
	mat het_plot_bar[7,1]=r(estimate)
	mat het_plot_bar[7,2]=r(se)

//Blanks
mat het_plot_bar[8,1]=0
mat het_plot_bar[8,2]=0		

//Econ Disadvan
qui lincom (1.cc_all_2013+1.cc_all_2013#1.slunch)
	mat het_plot_bar[9,1]=r(estimate)
	mat het_plot_bar[9,2]=r(se)

*Creating Variables from Matrix
svmat het_plot_bar
gen model=_n

gen hi_mhetero=het_plot_bar1+1.96*het_plot_bar2
gen low_mhetero_mhetero=het_plot_bar1-1.96*het_plot_bar2

replace model=. if _n>9
*Graphing
graph two bar het_plot_bar1 model if model==1, color(blue*.75) || ///
		rcap hi_mhetero low_mhetero_mhetero model if model==1 , lcolor(black) || ///
	bar het_plot_bar1 model if model==3, color(red*.75) || ///
		rcap hi_mhetero low_mhetero_mhetero model if model==3 , lcolor(black)  || ///
	bar het_plot_bar1 model if model==5, color(green*.75) || ///
		rcap hi_mhetero low_mhetero_mhetero model if model==5 , lcolor(black)  || ///
	bar het_plot_bar1 model if model==7, color(purple) || ///
		rcap hi_mhetero low_mhetero_mhetero model if model==7 , lcolor(black) ///
	, xlabel(1 "White" 3 "Black" 5 "Hispanic" ///
	7 "Asian" , angle(forty_five) labs(medsmall)  labc(black)) ///
	legend(off) ///
	xtitle("") ytitle("NAEP (Standardized)")  xlabel( , labc(black)) ylabel(-.06(.02).32, labs(vsmall) labc(black) )
	
graph save "${path}output\heter_bar_simple_d`i'.gph", replace
graph export "${path}output\heter_bar_simple_d`i'.png", as(png) replace
drop het_plot_bar* model hi_mhetero low_mhetero_mhetero
}
graph combine "${path}output\heter_bar_simple_d1.gph" ///
              "${path}output\heter_bar_simple_d2.gph" ///
			  "${path}output\heter_bar_simple_d3.gph" ///
              "${path}output\heter_bar_simple_d4.gph", row(2) col(2)
graph export "${path}output\heter_bar_simple_d_all.png", as(png) replace
********************************************************************************
********************************************************************************
//Table 2A. Differential Effects of Common Core for Academically Vulnerable Students
esttab hetero_dfe_wcov_1 ///
	   hetero_dfe_wcov_2 ///
       hetero_dfe_wcov_3 ///
       hetero_dfe_wcov_4 ///
	using "${path}output\heter_indiv.csv", ///
	title("Table 2A. Differential Effects of Common Core for Academically Vulnerable Students") ///
	coeflabel(1.cc_all_2013 "CC 2013" ///
			  1.cc_all_2013#1.slunch "CC 2013 x FRPL" ///
			  1.cc_all_2013#1.race "CC 2013 x Black" ///
			  1.cc_all_2013#2.race "CC 2013 x Hispanic" ///
			  1.cc_all_2013#3.race "CC 2013 x Asian" ///
			  1.cc_all_2013#4.race "CC 2013 x American Indian") ///
	label ///
	nodepvars ///
	b(3) ///
	t(3) ///
	ar2 (3) ///
	scalar(F) ///
	sfmt (2 0 0 0) ///
	replace  ///
	se(3) ///
	nomtitle ///
	nonote ///
	nogap ///
	noobs ///
	keep(1.cc_all_2013 1.cc_all_2013#1.slunch 1.cc_all_2013#1.race 1.cc_all_2013#2.race 1.cc_all_2013#3.race 1.cc_all_2013#4.race)
	
//Table 2B. Differential Effects of Common Core for LEP
local dset 1 2 3 4
foreach i of local dset{

local regvars i.sex i.iep i.slunch i.modage i.madeayp pv_lag1
qui reghdfe pv_1 i.cc_all_2011##i.lep i.cc_all_2013##i.lep ///
	`regvars' ///
	[pw=origwt] if data_set==`i', ///
	vce(cluster nces_sch) absorb(nces_dist_id year)
estimate store lep_het_dfe_wcov_`i'
}
esttab lep_het_dfe_wcov_1 lep_het_dfe_wcov_2 lep_het_dfe_wcov_3 lep_het_dfe_wcov_4 ///
	using "${path}output\lep_het_effects.csv", ///
	title("Table 2B. Differential Effects of Common Core for Academically Vulnerable Students") ///
	label ///
	nodepvars ///
	b(3) ///
	t(3) ///
	ar2 (3) ///
	scalar(F) ///
	sfmt (2 0 0 0) ///
	replace  ///
	se(3) ///
	nomtitle ///
	nogaps ///
	nonote ///
	keep(1.cc_all_2011 1.cc_all_2013 1.cc_all_2011#1.lep 1.cc_all_2013#1.lep)
********************************************************************************
********************************************************************************
//Figure 6. Common Core Effects by Race/Ethnicity and Economic Disadvantage
local dset 1 2 3 4
foreach i of local dset{
local regvars i.sex i.iep i.lep i.modage i.madeayp pv_lag1
qui reghdfe pv_1 i.cc_all_2011 i.cc_all_2013##i.slunch##i.race_2 i.cc_all_2013##i.slunch##i.race_3 i.cc_all_2013##i.slunch##i.race_4 i.race_5 i.race_6 `regvars' ///
[pw=origwt] if data_set==`i', ///
vce(cluster nces_sch) absorb(nces_dist_id year)
estimate store hete_dfe_int2_d`i'
}
//Bar Graph Heterogenous Effects by Race and Lunch (Intersectional Effects)
local dset 1 2 3 4
foreach i of local dset{

*Restoring Estimate
estimate restore hete_dfe_int2_d`i'

*Make a Blank Matrix
mat m_plot_bar=J(11,2,.)

//White Advantaged
qui lincom (1.cc_all_2013)
	mat m_plot_bar[1,1]=r(estimate)
	mat m_plot_bar[1,2]=r(se)
//White Disadvantaged
qui lincom (1.cc_all_2013+1.slunch+1.cc_all_2013#1.slunch) ///
-(1.slunch)
	mat m_plot_bar[2,1]=r(estimate)
	mat m_plot_bar[2,2]=r(se)

//Blanks
mat m_plot_bar[3,1]=0
mat m_plot_bar[3,2]=0	
	
//Black Advantaged
qui lincom (1.cc_all_2013+1.race_2+1.cc_all_2013#1.race_2)-(1.race_2)
	mat m_plot_bar[4,1]=r(estimate)
	mat m_plot_bar[4,2]=r(se)
	
//Black Disadvantaged
qui lincom (1.cc_all_2013+1.slunch+1.cc_all_2013#1.slunch+1.race_2+1.cc_all_2013#1.race_2+1.race_2#1.slunch+1.cc_all_2013#1.race_2#1.slunch) ///
-(1.slunch+1.race_2+1.race_2#1.slunch)
	mat m_plot_bar[5,1]=r(estimate)
	mat m_plot_bar[5,2]=r(se)
	
//Blanks
mat m_plot_bar[6,1]=0
mat m_plot_bar[6,2]=0	

//Hispanic Advantaged
qui lincom (1.cc_all_2013+1.race_3+1.cc_all_2013#1.race_3)-(1.race_3)
	mat m_plot_bar[7,1]=r(estimate)
	mat m_plot_bar[7,2]=r(se)
//Hispanic Disadvantaged
qui lincom (1.cc_all_2013+1.slunch+1.cc_all_2013#1.slunch+1.race_3+1.cc_all_2013#1.race_3+1.race_3#1.slunch+1.cc_all_2013#1.race_3#1.slunch) ///
-(1.slunch+1.race_3+1.race_3#1.slunch)
	mat m_plot_bar[8,1]=r(estimate)
	mat m_plot_bar[8,2]=r(se)
//Blanks
mat m_plot_bar[9,1]=0
mat m_plot_bar[9,2]=0	

//Asian Advantaged
qui lincom (1.cc_all_2013+1.race_4+1.cc_all_2013#1.race_4)-(1.race_4)
	mat m_plot_bar[10,1]=r(estimate)
	mat m_plot_bar[10,2]=r(se)
//Asian Disadvantaged
qui lincom (1.cc_all_2013+1.slunch+1.cc_all_2013#1.slunch+1.race_4+1.cc_all_2013#1.race_4+1.race_4#1.slunch+1.cc_all_2013#1.race_4#1.slunch) ///
-(1.slunch+1.race_4+1.race_4#1.slunch)
	mat m_plot_bar[11,1]=r(estimate)
	mat m_plot_bar[11,2]=r(se)

*Creating Variables from Matrix
svmat m_plot_bar
gen model=_n

gen hi_hetero=m_plot_bar1+1.96*m_plot_bar2
gen low_hetero_hetero=m_plot_bar1-1.96*m_plot_bar2

replace model=. if _n>11
*Graphing
graph two bar m_plot_bar1 model if model==1, color(ltblue) || ///
		rcap hi_hetero low_hetero_hetero model if model==1 , lcolor(black) || ///
	bar m_plot_bar1 model if model==2, color(blue*.75) || ///
		rcap hi_hetero low_hetero_hetero model if model==2 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==4, color(red*.5) || ///
		rcap hi_hetero low_hetero_hetero model if model==4 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==5, color(red*.75) || ///
		rcap hi_hetero low_hetero_hetero model if model==5 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==7, color(green) || ///
		rcap hi_hetero low_hetero_hetero model if model==7 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==8, color(eltgreen) || ///
		rcap hi_hetero low_hetero_hetero model if model==8 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==10, color(purple) || ///
		rcap hi_hetero low_hetero_hetero model if model==10 , lcolor(black)  || ///
	bar m_plot_bar1 model if model==11, color(lavender) || ///
		rcap hi_hetero low_hetero_hetero model if model==11 , lcolor(black)  ///
	, xlabel(1 "Advantaged, White" 2 "Disadvantaged, White" ///
	4 "Advantaged, Black" 5 "Disadvantaged, Black" ///
	7 "Advantaged, Hispanic" 8 "Disadvantaged, Hispanic" ///
	10 "Advantaged, Asian" 11 "Disadvantaged, Asian" ///
	, angle(forty_five) labs(medsmall)) ///
	legend(off) ///
	xtitle("") ytitle("NAEP (Standardized)")  xlabel( , labc(black)) ylabel(-.2(.05).4, labs(vsmall) labc(black))
graph save "${path}output\hetero_bar_ATE_pooled_d`i'.gph", replace
graph export "${path}output\hetero_bar_ATE_pooled_d`i'.png", as(png) replace
drop m_plot_bar* model hi_hetero low_hetero_hetero
}
graph combine "${path}output\hetero_bar_ATE_pooled_d1.gph" ///
              "${path}output\hetero_bar_ATE_pooled_d2.gph" ///
			  "${path}output\hetero_bar_ATE_pooled_d3.gph" ///
              "${path}output\hetero_bar_ATE_pooled_d4.gph", row(2) col(2)
graph export "${path}output\hetero_bar_ATE_pooled_d_all.png", as(png) replace
********************************************************************************
********************************************************************************
//Table 3. Effect of CC on Student Absences
local regvars i.sex i.iep i.lep i.slunch i.race_detail i.modage i.madeayp pv_lag1
oprobit day_absent cc_all_2011 cc_all_2013 ///
	 i.year i.data_set i.fips [pw=origwt], ///
	vce(cluster nces_sch)
estimate store abs_oprob_sfe_noc_low

oprobit day_absent cc_all_2011 cc_all_2013 ///
	 i.year `regvars' i.data_set i.fips [pw=origwt], ///
	vce(cluster nces_sch)
estimate store abs_oprob_sfe_wcov_low

esttab abs_oprob_sfe_noc_low abs_oprob_sfe_wcov_low   ///
	using "${path}output\absent_oprobit.csv", ///
	title("Effect on School Absences") ///
	label ///
	nodepvars ///
	b(3) ///
	t(3) ///
	ar2 (3) ///
	scalar(F) ///
	sfmt (2 0 0 0) ///
	replace  ///
	se(3) ///
	nomtitle ///
	nogaps ///
	nonote ///
	keep(cc_all_2011 cc_all_2013)
********************************************************************************
********************************************************************************
//Table 4. Pre-Treatment Balance on Education Reform Capacity
cls
clear
//Ideology- Short Mcarty Data
use "${path}data\shor mccarty 1993-2014 state aggregate data public June 2015.dta", clear
keep if inrange(year,2004,2010)
bysort fips: egen hou_sum=total(hou_chamber)
bysort fips: egen sen_sum=total(sen_chamber)
gen st_ideo=(hou_sum+sen_sum)/2
duplicates drop fips st_ideo, force
keep fips st_ideo
tempfile st_ideo
save "`st_ideo'"

//Adoption Method from NCSL
import delimited "${path}data\adoption_method.csv", varnames(1) encoding(UTF-8) clear
encode  adoption_method, gen(adopt_institution)
drop adoption_method
rename state statename
tempfile adoption_method
save "`adoption_method'"

//Pre Common Core Rigor Carmichael et al 2010 from Fordham
import delimited "${path}data\pre_cc_standards.csv", varnames(1) encoding(UTF-8) clear 
encode  ela_2010, gen(ela_rigor)
encode  math_2010, gen(math_rigor)
keep statename ela_rigor math_rigor aft*
egen min_rigor=rowmin(ela_rigor math_rigor)
recode ela_rigor math_rigor (3=4) (4=3)
tempfile pre_rigor
save "`pre_rigor'"

//Initial consortia membership in 2010 from Salazar 2010
import delimited "${path}data\initial_consortia_membership.csv", varnames(1) encoding(UTF-8) clear
encode  consortia_initial, gen(initial_membership)
drop consortia_initial
rename state statename
tempfile initial_consort
save "`initial_consort'"

//Used CC test in 2015 and evener exit all CC consortia Salazar 2010 Miller 2015 and 2018
import delimited "${path}data\cc_test_and_exit.csv", encoding(UTF-8) clear
tempfile cc_test
save "`cc_test'"

//State Finance from ELSI
import delimited "${path}data\st_finance.csv", encoding(UTF-8) clear
tempfile st_finance
save "`st_finance'"

//Ed Gov Data from Henig 2011
use "${path}data\nga_gov_2000_2010.dta", clear
bysort fips: egen eg_nga_total=total(ed_nga)
keep icspr eg_nga_total
duplicates drop  icspr eg_nga_total, force
tempfile ed_gov
save "`ed_gov'"

//SEA Staff Brown, C. G., Hess, F. M., Lautzenheiser, D., & Owen, I. (2011). State education agencies as agents of change. Center for American Progress, American Enterprise Institute for Public Policy Research and The Board Foundation.
import delimited "${path}data\sea_staff.csv", encoding(UTF-8) clear
rename state statename
tempfile sea_staff
save "`sea_staff'"

//Outcome Difference between modal adoption (2011) and miminum implementation ela/math
import delimited "${path}data\min_diff_outcome.csv", encoding(UTF-8) clear
rename state statename
tempfile min_diff_outcome
save "`min_diff_outcome'"

//School Accountability
import excel "${path}data\State Accountability System Data.xlsx", sheet("Data") firstrow clear
gen amo_lin=0
	replace amo_lin=1 if AMO_Timeline_Structure=="Linear"
gen amo_step=0
	replace amo_step=1 if AMO_Timeline_Structure=="Stair step"
destring AMO*, force replace
egen amo_mean=rowmean(AMO*)
keep if year==2009
rename *, lower
destring math_4_proficiency math_8_proficiency read_4_proficiency read_8_proficiency, force replace
egen performance_stand_m=rowmean(math_4_proficiency math_8_proficiency)
egen performance_stand_r=rowmean(read_4_proficiency read_8_proficiency)
keep statename amo_mean amo_lin amo_step  performance_stand_m performance_stand_r growthwaiver
tempfile school_account
save "`school_account'"

//ESEA Waiver
import excel "${path}data\State Accountability System Data.xlsx", sheet("Data") firstrow clear
keep if year==2013
keep statename ESEAWaiver
rename *, lower
tempfile esea_waiver
save "`esea_waiver'"

//State Baker Data (http://www.schoolfundingfairness.org/data-download)
use "${path}data\State_Panel_Complete.dta", clear
rename statefip fips
keep if year==2002
keep fips inc_effort fairness tchsalary31_40 nontchsal31_40
tempfile st_baker
save "`st_baker'"

//District Baker Data (http://www.schoolfundingfairness.org/data-download)
use "${path}data\PUBLIC_LEApanel1.17_trunc.dta", clear
rename fips_saipe fips
keep fips mh_income2000_edge mhu_value2000_edge t06_f33full
collapse (mean) mh_income2000_edge mhu_value2000_edge t06_f33full, by(fips)
destring fips, force replace
drop if fips==.
tempfile lea_baker
save "`lea_baker'"

*Merging Data
use "${path}data\state_crosswalk.dta", clear
merge 1:1 fips using "`st_ideo'", nogen
merge 1:1 statename using "`adoption_method'", nogen
merge 1:1 statename using  "`pre_rigor'", nogen
merge 1:1 statename using  "`initial_consort'", nogen
merge 1:1 statename using  "`cc_test'", nogen
merge 1:1 statename using  "`st_finance'", nogen
merge 1:1 icspr using  "`ed_gov'", nogen
merge 1:1 fips using  "`st_baker'", nogen
merge 1:1 fips using  "`lea_baker'", nogen
merge 1:1 statename using  "`sea_staff'", nogen
merge 1:1 statename using  "`min_diff_outcome'", nogen
merge 1:1 statename using  "`school_account'", nogen
merge 1:1 statename using  "`esea_waiver'", nogen
merge 1:1 fips using "${path}data\anes_2012.dta", nogen
merge 1:1 statename using  "${path}data\exclude_ever.dta", nogen

*Making New Vars
gen sbe=adopt_institution
recode sbe (1=1) (2=0) (3=0) (4=0)
replace ppe=ppe/1000
tab initial_membership, gen(init_consort)

gen cc_ever=min_cc 
recode cc_ever (2012=1) (2013=1) (2014=0) (2015=0) (.=0)

local vars per_state_rev ppe sea_staff st_ideo eg_nga_total ed_spend_support ///
amo_mean performance_stand_m performance_stand_r ///
inc_effort state_chartershare fairness tchsalary31_40 nontchsal31_40 ///
mh_income2000_edge mhu_value2000_edge t06_f33full

foreach i of local vars{
egen sd_`i'=std(`i')
drop `i'
rename sd_`i' `i'
}
recode ela_rigor math_rigor (1=1) (2=1) (3=1) (4=0) (5=0) (6=0) (7=0)
//Correcting DC
local change amo_mean aft_low_ela_e aft_low_ela_m aft_low_math_e aft_low_math_m
foreach i of local change{
replace `i'=. if fips==11
}
local vars per_state_rev ///
		   ppe ///
		   inc_effort ///
		   fairness ///
		   tchsalary31_40 ///
		   nontchsal31_40 ///
		   mh_income2000_edge ///
		   mhu_value2000_edge ///
		   t06_f33full ///
		   sbe ///
		   sea_staff ///
		   st_ideo ///
		   eg_nga_total ///
		   ed_spend_support ///
		   init_consort1 ///
		   init_consort3 ///
		   init_consort4 ///
		   ever_exit_all_consortia ///
		   cc_test_2015 ///
		   amo_mean ///
		   amo_lin ///
		   amo_step ///
		   performance_stand_m ///
		   performance_stand_r ///
		   eseawaiver ///
		   growthwaiver ///
		   ela_rigor ///
		   math_rigor  ///
		   aft_low_ela_e  ///
		   aft_low_ela_m  ///
		   aft_low_math_e  ///
		   aft_low_math_m 
	  

putexcel set "${path}output\capacity_balance.xlsx", modify
local row=3
foreach i of local vars{

qui reg cc_ever `i'
estimate store mod_`i'
putexcel C`row'=_b[`i']
mat p_`i'=2*ttail(e(df_r), abs(_b[`i']/_se[`i']))
putexcel D`row'=matrix(p_`i')
mat n=e(N)
putexcel E`row'=matrix(n)

qui reg cc_ever `i' if exclude_ever==0
estimate store exc_`i'
putexcel G`row'=_b[`i']
mat p_`i'=2*ttail(e(df_r), abs(_b[`i']/_se[`i']))
putexcel H`row'=matrix(p_`i')
mat n=e(N)
putexcel I`row'=matrix(n)

local row=`row'+1
}
********************************************************************************
********************************************************************************
log close _all
