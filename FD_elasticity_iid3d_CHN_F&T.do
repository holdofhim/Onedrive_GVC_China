
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_elasticity_iid3d_CHN, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
keep if iid=="TOT"
drop if cid=="CHN" | cid=="ROW"
joinby cid using country-codes
sort year e_va_all
bysort year: gen rank_rev_1995=_n if year==1995
sort cid year
bysort cid: replace rank_rev_1995=rank_rev_1995[_n-1] if rank_rev_1995==.


* bar graphs
* 1. 
*tw bar e_va_all year, by(n, compact) xsize(8) ysize(13)

preserve
keep if inlist(year,1995,2000,2005,2010)
keep if inlist(cid,"KOR","DEU","USA","JPN","TWN","VNM")
#delimit ;
graph bar (asis) e_va_all,
           over(country, label(labsize(vsmall)) sort(rank_rev_1995)) over(year, label(labsize(vsmall)))
		   blabel(bar, pos(upper) format(%9.3f) size(vsmall))
		   asyvars legend(size(small) row(1) order(5 1 2 3 6 4))
		   ytitle("")
		   xsize(12) ysize(7)
		   ;
#delimit cr
restore


* Data Export

gsort year -e_va_all
bysort year: gen rank=_n

keep if inlist(year,1995,2010)
keep year country rank e_va_all
order year country rank e_va_all

cd D:\OneDrive\GVC_China\draft
dataout, save(FD_elasticity_iid3d_CHN_y2009.tex) tex replace
