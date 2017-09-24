
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_Real_Growth_Effect_iid3d_CHN_2009-2011, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
replace year = substr(year,6,4)
destring year, replace
keep if iid=="TOT"
drop if cid=="CHN" | cid=="ROW"
sort year gr_va_all
bysort year: gen rank_rev_1995=_n if year==1995
sort cid year
bysort cid: replace rank_rev_1995=rank_rev_1995[_n-1] if rank_rev_1995==.
format gr_* %9.2f


* bar graphs

preserve
keep if inlist(cid,"MEX","CAN","GBR","KOR","TWN","MYS","USA","VNM","CHL")
#delimit ;
graph bar (asis) gr_va_all,
           over(cid, label(labsize(medsmall)) sort(rank_rev_1995)) over(year, label(labsize(small)))
		   blabel(bar, pos(upper) format(%9.2f) size(vsmall))
		   asyvars legend(size(small) row(1))
		   ytitle("") ylabel("")
		   xsize(12) ysize(8)
		   ;
#delimit cr
restore


* Data Export for China

loc cty "CHN"

cd D:\OneDrive\GVC_China\Data
use FD_Real_Growth_Effect_iid3d_`cty'_2009-2011, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
replace year = substr(year,6,4)
destring year, replace
keep if iid=="TOT"
joinby cid using country-codes
replace country="Korea" if country=="South Korea"
format gr_* %9.2f

cd D:\OneDrive\GVC_China\draft
keep year cid country gr_va_*
order year cid country gr_va_*

keep if inlist(cid,"TWN","MYS","KOR","VNM","CHL","RUS","IDN","AUS") ///
	  | inlist(cid,"IND","JPN","CAN","DEU","ITA","GBR","USA","FRA") ///
	  | inlist(cid,"BRA","ESP","MEX","PHL","CHN")
drop if cid=="`cty'"

keep year country gr_*
reshape wide gr*, i(country) j(year)
rename gr_va_* *
local group "ndura dura ucon svc all"
foreach x of local group {
	egen `x'_avg = rowmean(`x'*) 
	}
format *avg %9.2f
gsort -all_avg
keep  country all2009 all2010 all2011 ndura_avg dura_avg ucon_avg svc_avg all_avg
order country all2009 all2010 all2011 ndura_avg dura_avg ucon_avg svc_avg all_avg
	
dataout, save(FD_Real_Growth_Effect_iid3d_`cty'.tex) dec(2) tex replace

