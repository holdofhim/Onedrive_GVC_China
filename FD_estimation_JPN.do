
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_by_cid_2008-2011, clear
keep year ciid FD_JPN
destring *, replace

gen iid = substr(ciid,-3,.)
collapse FD_JPN, by(year iid)
gen countrycode = "JPN"

joinby year countrycode using pwt90
gen DFD = FD_JPN*xr
keep year iid DFD
reshape wide DFD, i(iid) j(year)

export excel using FD_estimation_JPN.xlsx, sheet("FD_ICIO") sheetrep first(var)
