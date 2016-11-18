
clear all
set more off
cd C:\OneDrive\GVC_China\Data

use FD_elasticity_iid3d_CHN, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
keep if iid=="TOT"
drop if cid=="CHN"
tw bar e_va_all year if year==2011, by(cid)
