**** The health effects of wage setting institutions: How collective bargaining improves health but not because it reduces inequality 
**** Sociology of health and illness

**** Aaron Reeves 

**** Variable construction file

/* 
Note:
A separate set of code files will replicate the analysis 

For further details on database construction email: aaron.reeves@spi.ox.ac.uk

*/

clear all

**** Import data - wage_setting_mortality_SHI_20210408.csv
import delimited "C:\Users\areeves\Dropbox\Wage setting and health\Replication\wage_setting_mortality_SHI_20210408.csv"

***************** Recoding variables
tsset idn year
*** Creating a new min wage variable
recode minwage (1/4=0) (5/7=1) (8 0=2), g(min_wage4)
revrs min_wage4
tab revmin_wage, g(wage4_new)

recode minwage (1/4=1) (0 5/8=0), g(min_wage2_new)


tab wcoord, g(wcoord)
tab year, g(year)

gen plt15_pc = plt15/pop
gen po65_pc = po65/pop

gen smr_adult_wbi = smr_adult_male_wbi+smr_adult_female_wbi

*** Economic indicators
foreach var of varlist  gdppp {
	gen `var'_cap = (`var'*1000000)/(pop*1000)
	}

	
	*** transformations
foreach var of varlist pop gdppp_cap{
	gen log_`var' = log(`var') if `var'!=. 
	}


** Health variables
foreach var of varlist smr_adult_wbi smr_under5_wbi lifexp_wbi smr_trans_who smr_falls_who {
	gen log_`var' = log(`var') if `var'!=. & `var'>0
	}
gen health = totheal/gdpncu

*** deaths in wwii
gen wwii = 0.58 if idn==1
replace wwii = 5.56 if idn==2
replace wwii = 1.05 if idn==3
replace wwii = 0.38 if idn==4
replace wwii = 0.16 if idn==5
replace wwii = 2.57 if idn==6
replace wwii = 1.44 if idn==7
replace wwii = 8.23 if idn==8
replace wwii = 11.17 if idn==9
replace wwii = 0.001 if idn==10
replace wwii = 1.16 if idn==11
replace wwii = 4.34 if idn==12
replace wwii = 1.69 if idn==13
replace wwii = 2.41 if idn==14
replace wwii = 0.72 if idn==15
replace wwii = 0.35 if idn==16
replace wwii = 0 if idn==17
replace wwii = 0 if idn==18
replace wwii = 0.03 if idn==19
replace wwii = 0 if idn==20
replace wwii = 0.94 if idn==21
replace wwii = 0.32 if idn==22


*** average temperature
gen temp = 	21.65 if idn==1
replace temp = 6.35 if idn==2
replace temp = 9.55 if idn==3
replace temp = -5.35 if idn==4
replace temp = 7.50 if idn==5
replace temp = 1.70 if idn==6
replace temp = 10.70 if idn==7
replace temp = 8.50 if idn==8
replace temp = 15.40 if idn==9
replace temp = 9.30 if idn==10
replace temp = 13.45 if idn==11
replace temp = 11.15 if idn==12
replace temp = 8.65 if idn==13
replace temp = 9.25 if idn==14
replace temp = 10.55 if idn==15
replace temp = 1.50 if idn==16
replace temp = 15.15 if idn==17
replace temp = 13.30 if idn==18
replace temp = 2.10 if idn==19
replace temp = 5.50 if idn==20
replace temp = 8.45 if idn==21
replace temp = 8.55 if idn==22


*** civil (1) or common law (0)
gen law = 	0 if idn==1
replace law = 1 if idn==2
replace law = 1 if idn==3
replace law = 0 if idn==4
replace law = 1 if idn==5
replace law = 1 if idn==6
replace law = 1 if idn==7
replace law = 1 if idn==8
replace law = 1 if idn==9
replace law = 0 if idn==10
replace law = 1 if idn==11
replace law = 1 if idn==12
replace law = 1 if idn==13
replace law = 1 if idn==14
replace law = 0 if idn==15
replace law = 0.5 if idn==16
replace law = 1 if idn==17
replace law = 1 if idn==18
replace law = 1 if idn==19
replace law = 1 if idn==20
replace law = 0 if idn==21
replace law = 0 if idn==22



*** pr (1) or majoritarian (0)
gen pr = 	0.5 if idn==1
replace pr = 1 if idn==2
replace pr = 1 if idn==3
replace pr = 0 if idn==4
replace pr = 1 if idn==5
replace pr = 1 if idn==6
replace pr = 0 if idn==7
replace pr = 0.5 if idn==8
replace pr = 1 if idn==9
replace pr = 0.5 if idn==10
replace pr = 1 if idn==11
replace pr = 0.5 if idn==12
replace pr = 1 if idn==13
replace pr = 1 if idn==14
replace pr = 0.5 if idn==15
replace pr = 1 if idn==16
replace pr = 1 if idn==17
replace pr = 1 if idn==18
replace pr = 1 if idn==19
replace pr = 1 if idn==20
replace pr = 0 if idn==21
replace pr = 0 if idn==22


*** country dummies
tab idn, g(idn)

*** sample
qui reg log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord gdppp_cap  ///
	sstran  leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year 
gen sample_smr =1 if e(sample)==1

qui reg log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord gdppp_cap  ///
	sstran  leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year 
gen sample_inf =1 if e(sample)==1

qui reg log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord gdppp_cap  ///
	sstran  leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year 
gen sample_life =1 if e(sample)==1


*** exporting data for causal sensitivity analysis 
export delimited using "C:\Users\areeves\Dropbox\Wage setting and health\Replication\sens_smr.csv" if log_smr_adult_wbi!=., nolabel replace
export delimited using "C:\Users\areeves\Dropbox\Wage setting and health\Replication\sens_life.csv" if log_lifexp_wbi!=., nolabel replace
export delimited using "C:\Users\areeves\Dropbox\Wage setting and health\Replication\sens_inf.csv" if log_smr_under5_wbi!=., nolabel replace

export delimited using "C:\Users\areeves\Dropbox\Wage setting and health\Replication\wageR2.csv", nolabel replace



