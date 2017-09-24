
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_Real_Growth_Effect_iid3d_CHN_by_year, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
keep if inlist(cid,"CHL","KOR","VNM","SAU")
keep if inlist(year,1995,2010)
keep year cid iid e_va_all
reshape wide e, i(cid iid) j(year)
reshape wide e*, i(iid) j(cid) s
rename e_va_all1995* e1995*
rename e_va_all2010* e2010*
format e* %9.2f
order iid e1995* e2010*


* bar graphs
preserve
keep if inlist(year,1995,2000,2005,2010)
keep if inlist(cid,"KOR","DEU","USA","JPN","TWN","VNM")
#delimit ;
graph bar (asis) e_va_all,
           over(cid, label(labsize(medsmall)) sort(rank_rev_1995)) over(year, label(labsize(small)))
		   blabel(bar, pos(upper) format(%9.2f) size(vsmall))
		   asyvars legend(size(small) row(1) order(5 1 2 3 6 4))
		   ytitle("") ylabel("")
		   xsize(12) ysize(8)
		   ;
#delimit cr
restore

