

* Real FD at 95 constant price

clear all
set more off
cd D:\OneDrive\GVC_China\data

import excel using FD_estimation_CHN.xlsx, sheet("FD_1995-2014") cellrange(A2:CC19) firstrow clear
keep ename iid FD1995_95-FD2011_95
rename FD*_95 FD*
gen dnd=.
replace dnd=1 if iid==100 | iid==211 | iid==212 | iid==213 | iid==214
replace dnd=2 if iid==221 | iid==222 | iid==223 | iid==224 | iid==225
replace dnd=3 if iid==310  // utilities & construction
replace dnd=4 if dnd==.

reshape long FD, i(iid) j(year)
drop iid
drop if FD==.
collapse(sum) FD, by(year dnd)
replace FD = FD/1000000 //trillion yuan
format FD %9.1fc
keep if inlist(year,1995,2000,2005,2008,2009,2010,2011)

* Total real FD
#delimit ;
twoway (connected FD year if dnd==1, lpattern(longdash) mlabel(FD) mlabp(12) mlabs(small))
       (connected FD year if dnd==2, lpattern(solid) mlabel(FD) mlabp(9) mlabs(small))
	   (connected FD year if dnd==3, lpattern(dash_dot) mlabel(FD) mlabp(3) mlabs(small))
	   (connected FD year if dnd==4, lpattern(dash) mlabel(FD) mlabp(12) mlabs(small)),
       legend(size(*.95) label(1 "Nondurables") label(2 "Durables") 
	   label(3 "Utilities & Construction") label(4 "Services") cols(2))
	   xlabel(1995 2000 2005 2008 2011,labsize(*.9)) xtitle("") 
	   ylabel(0(2)10, labsize(*.9) format(%9.0fc)) ytitle("")
	   ytitle("Trillion Yuan (constant 1995 prices)", size(*.95) margin(small))
	   xsize(11) ysize(8);
#delimit cr
graph export D:\OneDrive\GVC_China\Draft\CHN_FinalDemand_95-11_95p.eps, replace

*/



* Growth Trend by Product Group during the 2008-2011 period

clear all
set more off
import excel using "D:\onedrive\GVC_China\data\FD_estimation_CHN.xlsx", ///
			sheet("FD_by_Group") cellrange(A2:Z6) firstrow
keep group FDhat* gFD*
rename FDhat*_* FDhat*
gen id=_n
reshape long FDhat gFD, i(group) j(year) s
replace year="Average" if year=="avg"
format FDhat gFD %9.1fc

label define id 1 "Nondurables" 2 "Durables" 3 "Utilities & Construction" 4 "Services"
label values id id

* Connected Line type Figure
#delimit ;
twoway (connected FDhat year if id==1, lpattern(longdash) mlabel(FD) mlabp(6) mlabs(small))
       (connected FDhat year if id==2, lpattern(dash) mlabel(FD) mlabp(12) mlabs(small))
	   (connected FDhat year if id==3, lpattern(dash_dot) mlabel(FD) mlabp(3) mlabs(small))
	   (connected FDhat year if id==4, lpattern(solid) mlabel(FD) mlabp(12) mlabs(small)),
       legend(size(*.95) label(1 "Nondurables") label(2 "Durables") 
	   label(3 "Utilities & Construction") label(4 "Services") cols(2))
	   xlabel(2008(1)2011,labsize(*.9)) xtitle("") 
	   ylabel(5(5)25, labsize(*.9) format(%9.0fc)) 
	   ytitle("Annual Growth Rates (previous year's prices)", size(*.95) margin(small))
	   xsize(11) ysize(8);
#delimit cr
graph export D:\OneDrive\GVC_China\Draft\CHN_FinalDemand_08-11_pyp.eps, replace

* Bar type Figure
#delimit ;
graph bar (asis) FDhat,
           over(id, label(labsize(small))) over(year, label(labsize(medsmall)))
		   blabel(bar, pos(upper) format(%9.1f) size(small))
		   asyvars legend(size(medsmall) row(1))
	   	   ylabel("") 
		   ytitle("")
		   xsize(10) ysize(6);
#delimit cr
graph export D:\OneDrive\GVC_China\Draft\CHN_FinalDemand_08-11_pyp.eps, replace

* Bar type Contribution Figure
drop if year=="2008"
#delimit ;
graph hbar (asis) gFD, stack
           over(id, label(labsize(small))) over(year, label(labsize(medsmall)))
		   blabel(bar, pos(center) format(%9.1f) size(medsmall)) 
		   asyvars legend(size(medsmall) row(1))
		   intensity(*.95) linten(*1.5)
	   	   ylabel("") 
		   ytitle("")
		   xsize(12) ysize(7);
#delimit cr
graph export D:\OneDrive\GVC_China\Draft\CHN_FinalDemand_09-11_pyp.eps, replace

*/



/* real FD growth rate at t-1 price (annually)

clear all
set more off
import excel using "D:\KDI\GVC\ICIO\Excel\FD_estimation_CHN.xlsx", ///
			sheet("FD_1995-2014") cellrange(A2:DB20) firstrow
keep ename kname iid FDhat2009_08-FDhat2014_13
rename FDhat*_* gFD*
reshape long gFD, i(iid) j(year)
format gFD* %9.1fc
keep if inrange(year,2009,2011)
tempfile a b c d


* non-durable
#delimit ;
twoway (connected gFD year if iid==100, lpattern(longdash) mlabel(gFD) mlabp(6) mlabs(small))
       (connected gFD year if iid==211, lpattern(dash) mlabel(gFD) mlabp(6) mlabs(small))
	   (connected gFD year if iid==212, lpattern(dash_dot) mlabel(gFD) mlabp(12) mlabs(small))
	   (connected gFD year if iid==214, lpattern(solid) mlabel(gFD) mlabp(12) mlabs(small))
	   (connected gFD year if iid==310, lpattern(shortdash) mlabel(gFD) mlabp(3) mlabs(small)),
       legend(size(small) label(1 "AGR.MIN") label(2 "FOD") label(3 "TEX") label(4 "PET.CHM") 
	   label(5 "EGW.CON") rows(1))
	   xlabel(2009(1)2011) xtitle("") 
	   ylabel(-10(10)40, format(%9.0fc)) ytitle("")
	   xsize(10) ysize(10);
	   *saving(CHNGrowthRealFD_09-14_prev_trend_nondu, replace);
#delimit cr
graph save `a'.gph, replace

* durable
#delimit ;
twoway (connected gFD year if iid==221, lpattern(longdash) mlabel(gFD) mlabp(12) mlabs(small))
       (connected gFD year if iid==222, lpattern(dash) mlabel(gFD) mlabp(6) mlabs(small))
	   (connected gFD year if iid==223, lpattern(dash_dot) mlabel(gFD) mlabp(6) mlabs(small))
	   (connected gFD year if iid==224, lpattern(solid) mlabel(gFD) mlabp(12) mlabs(small))
	   (connected gFD year if iid==225, lpattern(shortdash) mlabel(gFD) mlabp(6) mlabs(small)),
       legend(size(vsmall) label(1 "MET.MEQ") label(2 "CEQ") label(3 "ELO") label(4 "MTR") label(5 "OTM") cols(5))
	   xlabel(2009(1)2011) xtitle("") 
	   ylabel(-10(10)40, format(%9.0fc)) ytitle("")
	   xsize(9) ysize(10);
	   *saving(CHNGrowthRealFD_09-14_prev_trend_du, replace);
#delimit cr
graph save `b'.gph, replace

* construction 
#delimit ;
twoway (connected gFD year if iid==310, mlabel(gFD) mlabp(12) mlabs(small)),
       legend(size(small) label(1 "EGW.CON") cols(1))
	   xlabel(2009(1)2011) xtitle("") 
	   ylabel(-10(10)40, format(%9.0fc)) ytitle("")
   	   xsize(9) ysize(10);
	   *saving(CHNGrowthRealFD_09-14_prev_trend_svc, replace);
#delimit cr
graph save `c'.gph, replace

* service
#delimit ;
twoway (connected gFD year if iid==321, lpattern(longdash) mlabel(gFD) mlabp(12) mlabs(small))
       (connected gFD year if iid==322, lpattern(dash) mlabel(gFD) mlabp(3) mlabs(small))
	   (connected gFD year if iid==323, lpattern(dash_dot) mlabel(gFD) mlabp(6) mlabs(small))
	   (connected gFD year if iid==324, lpattern(solid) mlabel(gFD) mlabp(6) mlabs(small))
	   (connected gFD year if iid==325, lpattern(shortdash) mlabel(gFD) mlabp(12) mlabs(small)),
       legend(size(small) label(1 "WTR.HTR") label(2 "TRN.PTL") ///
						  label(3 "FIN") label(4 "REA") label(5 "OTS") cols(6))
	   xlabel(2009(1)2011) xtitle("") 
	   ylabel(-10(10)40, format(%9.0fc)) ytitle("")
   	   xsize(9) ysize(10);
	   *saving(CHNGrowthRealFD_09-14_prev_trend_svc, replace);
#delimit cr
graph save `d'.gph, replace
graph combine `a'.gph `b'.gph `c'.gph `d'.gph, cols(2) xsize(9) ysize(10)

*/
