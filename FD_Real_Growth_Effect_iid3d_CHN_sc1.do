
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_Real_Growth_Effect_iid3d_CHN_sc1, clear
replace year = substr(year,6,4)
destring year gr*, replace
format gr_* %9.2f

drop if regexm(ciid,"[0-9]")
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
keep if year>=2009
keep if iid=="TOT"
drop if inlist(cid,"CHN","ROW","HKG","SGP")
gsort year -gr_va_all

bysort cid: egen avgr_va = mean(gr_va_all)
*collapse gr_va_all, by(cid)
gsort year -avgr
