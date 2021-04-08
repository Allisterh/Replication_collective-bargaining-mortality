
**** The health effects of wage setting institutions: How collective bargaining improves health but not because it reduces inequality 
**** Sociology of health and illness

**** Aaron Reeves 

**** Variable construction file

/* 
Note:
A seprate set of code files will replicate the analysis 

For further details on database construction email: aaron.reeves@spi.ox.ac.uk

*/


**** Import data 
**** Run variable creation file


*** Table 1
eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4  i.year || id: if year<2011, mle variance
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4  i.year || id: if year<2011, mle variance	 
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4  i.year || id: if year<2011, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)


*** Table 2
*** falsification analysis
eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if log_smr_trans_who!=. & year<2011, mle variance 
eststo: mixed log_smr_trans_who i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo: mixed log_smr_falls_who i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
esttab, replace nogap b(2) se(2) r2(2) compress drop(log_pop)


*** figures 1-3 are created in R - see other codes files for these analyses. 


*** Table 3
*** Heckman selection model

*** web apendix 8 - first stage of heckman selection model
eststo clear
eststo: probit min_wage2_new ud unioncent cent i.wcoord log_gdppp_cap  sstran  leftseat  govexp log_pop temp wwii law pr i.year
esttab, replace nogap b(3) se(3) r2(2) compress drop(log_pop)


predict xb, xb
gen mills = normalden(xb)/normal(xb)
summ mills
gen mills_MC = (mills-r(mean))/2*r(sd)

eststo clear 
eststo: mixed log_smr_adult_wbi i.min_wage2_new ud unioncent cent i.wcoord log_gdppp_cap  sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year mills_MC || id: , mle variance
eststo: mixed log_lifexp_wbi i.min_wage2_new ud unioncent cent i.wcoord log_gdppp_cap  sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year mills_MC || id: , mle variance
eststo: mixed log_smr_under5_wbi i.min_wage2_new ud unioncent cent i.wcoord log_gdppp_cap  sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year mills_MC || id: , mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)




*** Table 4	
*** Fixed-effect models
eststo clear
eststo: xtreg log_smr_adult_wbi revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap  sstran  leftseat ///
	health   govexp tmedcv  pop plt15_pc po65_pc i.year , fe cluster(id)
eststo: xtreg log_lifexp_wbi revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap  sstran  leftseat health   ///
	govexp tmedcv  pop plt15_pc po65_pc i.year  , fe cluster(id)
eststo: xtreg log_smr_under5_wbi revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap  sstran  leftseat ///
	health   govexp tmedcv  pop plt15_pc po65_pc i.year  , fe cluster(id)
esttab, replace nogap b(3) se(3) r2(2) compress drop(pop)


*** figures 4-6 are also created using R. see aditional code for those files. 




*** web appendix 1
tsline revmin_wage4 if year<2011, by(id, note(" ")) xtitle(Year) ytitle(Wage setting policies) ///
	ylabel(1 "No minimum wage" 2 "Minimum wage" 3 "Collective bargaining" )  xlabel(1960(10)2010, angle(45))

	
	
	
*** web appendix 3 - without logged values	
eststo clear
eststo: mixed smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo: mixed lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo: mixed smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)
	

	
	
*** web appendix 4 - Dropping United States

eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=22, mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=22, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=22, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)	




*** web appendix 5 - Dropping Germany

eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=8, mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=8, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & idn!=8, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)



*** web appendix 6 - removing GFC
eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2007, mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2007, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2007, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)



*** web appendix 7 Model robustness

mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m1	
mixed log_smr_adult_wbi i.revmin_wage4 ud log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m2	
mixed log_smr_adult_wbi i.revmin_wage4 unioncent log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m3	
mixed log_smr_adult_wbi i.revmin_wage4 cent log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m4	
	mixed log_smr_adult_wbi i.revmin_wage4 i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m5	
	mixed log_smr_adult_wbi i.revmin_wage4 log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
eststo m6	


coefplot m6 || m2 || m3 || m4 || m5 || m1 , bycoefs ///
	keep(3.revmin_wage4 ) xtitle("Difference in standardized mortality rate (per 100,000)" "between collective bargaining and no minimum wage") ///
	ylabel(1 "No wage setting covariates" 2 "Union density"  3 "Union Centralization" 4 "Centralized bargaining" ///
	5 "Wage coordination" 6 "All wage setting covariates") 
	
	
	
	
*** web appendix 10 - missingness

*** excluding variables with a small number of obs
eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent  i.wcoord log_gdppp_cap sstran ///
	 leftseat health    tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 , mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent  i.wcoord log_gdppp_cap sstran  ///
	leftseat health    tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 , mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent  i.wcoord log_gdppp_cap sstran ///
	 leftseat health    tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)

*** excluding countries with a small number of obs

eststo clear
eststo: mixed log_smr_adult_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & exc_small!=1, mle variance
eststo: mixed log_lifexp_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran  ///
	leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & exc_small!=1, mle variance
eststo: mixed log_smr_under5_wbi i.revmin_wage4 ud unioncent cent i.wcoord log_gdppp_cap sstran ///
	 leftseat health   govexp tmedcv  log_pop plt15_pc po65_pc i.year || id: if year<2011 & exc_small!=1, mle variance
esttab, replace nogap b(3) se(3) r2(2) compress drop( log_pop)	
