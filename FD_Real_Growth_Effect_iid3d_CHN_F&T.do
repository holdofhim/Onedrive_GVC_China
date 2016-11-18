
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_Real_Growth_Effect_iid3d_CHN_1-year, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
replace year = substr(year,6,4)
destring year, replace
keep if iid=="TOT"
keep if year==2009
drop if cid=="CHN" | cid=="ROW"
gsort -gr_va_all
gen n=_n

joinby cid using country-codes

