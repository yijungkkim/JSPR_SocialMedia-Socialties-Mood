rename _all, lower
set more off


/////////////////////////////////////////////////
// descriptives 
/////////////////////////////////////////////////

tabstat age female married edu minority slfhlth1 glonelyavg socialconvoy_n ///
        if cidnum==1, stats(mean sd min max N) col(stat) long format(%5.2f)
tabstat age female married edu minority slfhlth1 glonelyavg socialconvoy_n ///
        if cidnum==1 & snsuser==1, stats(mean sd min max N) col(stat) long format(%5.2f)
tabstat age female married edu minority slfhlth1 glonelyavg socialconvoy_n ///
        if cidnum==1 & snsuser==0, stats(mean sd min max N) col(stat) long format(%5.2f)
		  

preserve
keep if cidnum==1
global tcont age edu slfhlth1 glonelyavg socialconvoy_n qneuro
global tcat female married minority 
forvalues i= 1/6 {
    local t: word `i' of $tcont
	ttest `t', by(snsuser)
}
forvalues i=1/3 {
	local w: word `i' of $tcat
	tab `w' snsuser, chi
}
restore

global tcont2 daysns aloneday dayphone
forvalues i=1/3 {
	local w: word `i' of $tcont2
	ttest `w', by(snsuser)
}

ttest daysns, by(female)

*correlations
tab edudum, gen(edu_) 

global dayvar weekend daysns dayinperson dayphone 
global personvar mweekend mdaysns mdayinperson mdayphone cohendiv socialconvoyc ///
				   age female married edu_1 edu_2 edu_3 minority slfhlth  
		  
eststo clear 
estpost correlate $dayvar negday posday, matrix
esttab using dayvar_corr.csv, c("b (fmt(2)) _star") unstack not noobs compress replace nolz
  
eststo clear // correlation
estpost correlate $personvar if cidnum==1, matrix
esttab using personvar_corr.csv, c("b (fmt(2)) _star") unstack not noobs compress replace nolz


//////////////////////////////////////////////////////
// analysis                                         
/////////////////////////////////////////////////////
global cov agec female married  ib0.edudum minority slfhlth  dweekend mweekend    // globals
global dcov agec female married  ib0.edudum minority slfhlth dweekend mweekend   // globals
global tvphone dayphone mdayphone 
global dtvphone ddayphone mdayphone 
global tvperson dayinperson mdayinperson 
global dtvperson ddayinperson mdayinperson 
global tipred cohendiv socialconvoyc

global lv1key ddayinperson ddayphone
global lv2key cohendiv socialconvoyc 
global lv1cov dweekend  
global lv2cov mdayinperson mdayphone mdaysns ///
			  agec female married  ib0.edudum minority slfhlth mweekend 

save "D:\10_DEWS\Mydata\social_media_paper\data\rr1.dta", replace



mixed negday c.ddaysns $lv1key $lv2key $lv1cov $lv2cov || cid:, vce(robust) 
esttab using neg1a.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson  ///
							  ddayphone cohendiv  ///
							  socialconvoyc) ///
						c("b (fmt(2)) _star se(par)") ///
					     refcat(ddaysns "sns" ddayinperson "social" ///
						 cohendiv "structure", nol) replace
						 
mixed negday c.ddaysns snssocial snsphone snscohen snsconvoy  ///
							  msnssocial msnsphone msnscohen msnsconvoy  ///
							  $lv1key $lv2key $lv1cov $lv2cov || cid:, vce(robust) 
							  
esttab using neg1b.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson snssocial ///
							  ddayphone snsphone cohendiv snscohen ///
							  socialconvoyc snsconvoy) ///
							  c("b (fmt(2)) _star se(par)") ///
							  refcat(ddaysns "sns" ddayinperson "social" ///
							  cohendiv "structure", nol) replace
							  
esttab using neg1b2.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson snssocial ddayphone ///
							  snsphone dweekend cohendiv snscohen msnscohen ///
							  socialconvoyc snsconvoy msnsconvoy ///
							  mdaysns mdayinperson msnssocial mdayphone msnsphone mweekend ///
							  agec female married 1.edudum 2.edudum minority slfhlth) ///
							  c("b (fmt(2)) _star se(par)") ///
							  refcat(ddaysns "day" cohendiv "cross" ///
							  mdaysns "person", nol) replace

mixed posday c.ddaysns  $lv1key $lv2key $lv1cov $lv2cov || cid:,
esttab using pos1a.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson ddayphone cohendiv  ///
							  socialconvoyc) c("b (fmt(2)) _star se(par)") ///
					     refcat(ddaysns "sns" ddayinperson "social" ///
						 cohendiv "structure", nol) replace
						 
esttab using pos1a2.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson ddayphone dweekend ///
							  cohendiv socialconvoyc mdaysns mdayinperson ///
							  mdayphone mweekend /// 
							  agec female married 1.edudum 2.edudum minority slfhlth) ///
							  c("b (fmt(2)) _star se(par)") ///
					     refcat(ddaysns "blank" ddayphone "blank" ///
								dweekend "blank" cohendiv "blank" ///
								socialconvoyc "blank" mdaysns "blank" ///
								mdayphone "blank" mweekend "blank", nol) replace
								
mixed posday c.ddaysns snssocial snsphone snscohen snsconvoy  ///
							  msnssocial msnsphone msnscohen msnsconvoy  ///
							  $lv1key $lv2key $lv1cov $lv2cov || cid:
							  
esttab using pos1b.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson snssocial ///
							  ddayphone snsphone cohendiv snscohen ///
							  socialconvoyc snsconvoy) ///
							  c("b (fmt(2)) _star se(par)") ///
							  refcat(ddaysns "sns" ddayinperson "social" ///
							  cohendiv "structure", nol) replace
							  
esttab using pos1b2.csv, noomit nonumber nobaselevels ///
						order(_cons ddaysns ddayinperson snssocial ddayphone ///
							  snsphone dweekend cohendiv snscohen msnscohen ///
							  socialconvoyc snsconvoy msnsconvoy ///
							  mdaysns mdayinperson msnssocial mdayphone msnsphone mweekend ///
							  agec female married 1.edudum 2.edudum minority slfhlth) ///
							  c("b (fmt(2)) _star se(par)") ///
							  refcat(ddaysns "day" cohendiv "cross" ///
							  mdaysns "person", nol) replace

							  
//////////////////////////////////////////////////////
// Figures                                        
/////////////////////////////////////////////////////

// Figure 1a
mixed negday c.daysns##c.dayinperson c.daysns##c.dayphone c.daysns##c.cohendiv  c.daysns##c.socialconvoyc $cov   mdaysns mdayinperson msnssocial mdayphone msnsphone || cid: , vce(cluster cid)
margins, dydx(daysns) at (dayinperson=(0 0.5 1))
margins, at (daysns=(0(0.2)1) dayinperson=(0 0.5 1))
marginsplot, scheme(s1mono)  recast(line) recastci(rarea) title ("1a: Daily negative mood") ytitle("Predicted level of daily negative mood", size(meds) margin(small)) ylabel(1(0.1)1.5) xtitle("Proportion of daily social media use", size(meds) margin(small)) legend(cols(1) size(small) order(4 "All day spent alone (b=0.07)" 5 "Half day spent alone (b=-0.04)" 6 "All day spent with someone (b=-0.15**)" ) ring(0) position(7) bmargin(small)) ciopts(fcolor(%50) lwidth(none)) 
graph export "D:\10_DEWS\Mydata\social_media_paper\data\R1Figure1a.tif", as(tif) name("Graph") replace

// Figure 1b
mixed posday c.daysns##c.dayinperson c.daysns##c.dayphone c.daysns##c.cohendiv  c.daysns##c.socialconvoy_n $cov   mdaysns mdayinperson msnssocial mdayphone msnsphone || cid: 
margins, dydx(daysns) at (socialconvoy_n= (9 15 22))
margins, at (daysns=(0(0.2)1) socialconvoy_n=(9 22))
marginsplot, scheme(s1mono)  recast(line) recastci(rarea) title ("1b: Daily positive mood") ytitle("Predicted level of daily positive mood", size(meds) margin(small))  ylabel(3.5(0.1)4.1) xtitle("Proportion of daily social media use", size(meds) margin(small)) legend(cols(1) size(small) order(3 "Smaller social network (b=0.22**)" 4 "Bigger social network (b=-0.07)") ring(0) position(5) bmargin(medium)) ciopts(fcolor(%50) lwidth(none)) 
graph export "D:\10_DEWS\Mydata\social_media_paper\data\R1Figure1b.tif", as(tif) name("Graph") replace



//////////////////////////////////////////////////////
// Supplementary Tables                                       
/////////////////////////////////////////////////////

// close tie vs weak tie
eststo clear 
eststo: mixed negday c.daysns c.ct_ipday c.wt_ipday c.ct_phday c.wt_phday ///
			          cohendiv socialconvoy_n ///
					  $cov mct_ipday mwt_ipday  mdaysns mct_phday mwt_phday  || cid:, vce(cluster cid)
esttab using suppneg1a.csv, noomit nonumber nobaselevels ///
						order(_cons daysns dayinperson  ///
							  dayphone cohendiv  ///
							  socialconvoyc) ///
						c("b (fmt(2)) _star se(par)") ///
					     refcat(daysns "sns" dayinperson "social" ///
						 cohendiv "structure", nol) replace
eststo clear
eststo: mixed negday c.daysns##c.ct_ipday c.daysns##c.wt_ipday c.daysns##c.ct_phday c.daysns##c.wt_phday ///
					  cohendiv socialconvoy_n ///
					  $cov mct_ipday mwt_ipday  mdaysns mct_phday mwt_phday   || cid:, vce(cluster cid)
esttab using suppneg1a.csv, noomit nonumber nobaselevels ///
						order(_cons daysns dayinperson  ///
							  dayphone cohendiv  ///
							  socialconvoyc) ///
						c("b (fmt(2)) _star se(par)") ///
					     refcat(daysns "sns" dayinperson "social" ///
						 cohendiv "structure", nol) replace

eststo clear			 
eststo: mixed posday c.daysns c.ct_ipday c.wt_ipday c.ct_phday c.wt_phday /// 
				      cohendiv socialconvoy_n ///
					  $cov mct_ipday mwt_ipday  mdaysns mct_phday mwt_phday  || cid:
esttab using supppos1a.csv, noomit nonumber nobaselevels ///
						order(_cons daysns dayinperson  ///
							  dayphone cohendiv  ///
							  socialconvoyc) ///
						c("b (fmt(2)) _star se(par)") ///
					     refcat(daysns "sns" dayinperson "social" ///
						 cohendiv "structure", nol) replace
						 					  
					  
eststo clear				  
eststo: mixed posday c.daysns##c.ct_ipday c.daysns##c.wt_ipday c.daysns##c.ct_phday c.daysns##c.wt_phday ///
					  cohendiv socialconvoy_n ///
					  $cov mct_ipday mwt_ipday  mdaysns mct_phday mwt_phday  || cid:
esttab using supppos1a.csv, noomit nonumber nobaselevels ///
						order(_cons daysns dayinperson  ///
							  dayphone cohendiv  ///
							  socialconvoyc) ///
						c("b (fmt(2)) _star se(par)") ///
					     refcat(daysns "sns" dayinperson "social" ///
						 cohendiv "structure", nol) replace


						 