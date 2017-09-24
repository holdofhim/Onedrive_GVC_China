clear all
set more off

cd C:\OneDrive\GVC_China\Data
use China_Import_Structure, clear
rename cid_np cid
destring *, replace

/*
gen f1995=100-mimsh1995
gen f2010=100-mimsh2010

reshape long mimsh fimsh f, i(cid) j(year)
keep if inlist(cid,"KOR","JPN","DEU","TWN","MYS","USA","VNM")

#delimit ;
graph bar mimsh f, 
	  over(year, lab(labsize(*.7))) over(cid, lab(labsize(*.85))) stack
	  legend(label(1 "Intermediate") label(2 "Final") size(*.9) row(1))
	  ylabel(,labsize(*.9))
	  blabel(bar, pos(center) format(%6.1f) size(vsmall))
	  xsize(12) ysize(7)
	  ;
#delimit cr
*/



*
reshape long mimsh fimsh f, i(cid) j(year)
gen row=0 if inlist(cid,"KOR","JPN","DEU","TWN","MYS","USA","VNM","CHN")
replace cid="ROW" if row==.
collapse (sum) fimsh*, by(year cid)
reshape wide fimsh, i(year) j(cid) s

#delimit ;
graph hbar fimsh*, 
	  over(year, lab(labsize(*.7))) stack
	  legend(size(*.9) row(1))
	  ylabel(,labsize(*.9))
	  blabel(bar, pos(center) format(%6.1f) size(vsmall))
	  xsize(12) ysize(7)
	  ;
#delimit cr

*graph export c:\OneDrive\GVC_China\Draft\CHN_Import_by_BEC.eps, replace
*/
