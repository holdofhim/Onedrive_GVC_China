
clear all
cd D:\OneDrive\GVC_China\Data

use pwt90.dta, clear
rename (countrycode pl_da) (cid deflator)
keep cid year deflator xr
tempfile deflator
save `deflator'

* Merge GDP deflator
use FD_by_cid, clear
gen iid = substr(ciid,-3,.)
destring year FD_* iid, replace
collapse (sum) FD_*, by(year iid)

reshape long FD_, i(year iid) j(cid) string
rename FD_ FD
sort year cid iid
joinby year cid using `deflator', unm(both)

preserve
keep if _m==2
drop xr
replace cid="ROW"
collapse deflator, by(cid year)
tempfile deflator_row
save `deflator_row'
restore

drop _merge
merge m:1 year cid using `deflator_row', update nogen
keep year cid iid FD deflator xr
sort year cid iid FD deflator xr
order year cid iid FD deflator xr
drop if iid==.

gen ciid = cid+"_"+string(iid)
reshape wide FD deflator xr, i(ciid) j(year)

* You have to adjust years to estimate
forval i=1996/2011 {
	loc j = `i'-1   // 1-year or 3-year difference
	loc k = substr(string(`j'),3,.)
	gen PI`i' = deflator`i'/deflator`j'
	gen FDhat`i'_`k' = (FD`i'/PI`i'/FD`j'-1)*100
	}

keep ciid FDhat*
save FDhat_World, replace
