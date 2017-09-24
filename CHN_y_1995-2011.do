clear all
set more off
cd c:\onedrive\GVC_china\data

use CHN_y_1995-2011, clear
gen regime = substr(ciid,5,3)
gen iid = substr(ciid,-3,.)
destring y* iid, replace
keep if inrange(iid,200,299)

collapse (sum) y*, by(regime)

reshape long y, i(regime) j(year)

sort year regime
bysort year: egen y_sh = pc(y)
bysort year: egen ex_sh = pc(y) if regime~="DOM"

format y_sh %9.1f

#delimit ;
twoway (connected y_sh year if reg=="DOM", lpattern(longdash) mlabel(y_sh) mlabp(6) mlabs(small))
       (connected y_sh year if reg=="NPR", lpattern(solid) mlabel(y_sh) mlabp(6) mlabs(small))
	   (connected y_sh year if reg=="PRO", lpattern(dash_dot) mlabel(y_sh) mlabp(6) mlabs(small)),
	   legend(label(1 "DOM") label(2 "NPR") label(3 "PRO") col(3))
	   xlabel(1995 2000 2005 2008 2011) xtitle("")
	   ylabel(, format(%9.0fc)) ytitle("")
	   xsize(11) ysize(8)
	   ;
delimit cr


graph bar (asis) y_sh, over(regime, label(labsize(small))) ///
over(year, label(labsize(small))) asyvars stack legend(size(small) row(1)) ///
ytick(0(10)100)
