
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use FD_elasticity_iid3d_CHN_by_year, clear
gen cid = substr(ciid,1,3)
gen iid = substr(ciid,-3,.)
keep if iid=="TOT"
drop if cid=="CHN" | cid=="ROW"
joinby cid using country-codes
sort year e_va_all
bysort year: gen rank_rev_1995=_n if year==1995
sort cid year
bysort cid: replace rank_rev_1995=rank_rev_1995[_n-1] if rank_rev_1995==.
replace country="Korea" if country=="South Korea"
format e_* %9.2f

* bar graphs

preserve
keep if inlist(year,2000,2005,2008,2009)
*keep if inlist(cid,"JPN","DEU","KOR","VNM","USA")
keep if inlist(cid,"JPN","IDN","KOR","VNM","PHL","USA","DEU")
#delimit ;
graph bar (asis) e_va_all,
           over(cid, label(labsize(medsmall)) sort(rank_rev_1995)) over(year, label(labsize(small)))
		   blabel(bar, pos(upper) format(%9.2f) size(vsmall))
		   asyvars legend(size(small) row(1))
		   ytitle("") ylabel("")
		   xsize(12) ysize(8)
		   ;
#delimit cr
restore
graph export D:\OneDrive\GVC_China\draft\FD_elasticity_CHN_allsectors.eps, replace


* Data Export

foreach cty in "CHN" "USA" {
	
	loc yr=2010
	
	cd D:\OneDrive\GVC_China\Data
	use FD_elasticity_iid3d_`cty'_by_year, clear
	gen cid = substr(ciid,1,3)
	gen iid = substr(ciid,-3,.)
	keep if iid=="TOT"
	joinby cid using country-codes
	replace country="Korea" if country=="South Korea"
	
	keep if inlist(year,1995,`yr')
	gsort year -e_va_all
	bysort year: gen rank`yr'=_n if year==`yr'
	gsort cid -year
	bysort cid: replace rank`yr'=rank`yr'[_n-1] if rank`yr'==.

	foreach z in "va" "mx" "fx" "ex" {
	
		cd D:\OneDrive\GVC_China\draft
		gsort year rank`yr'
		keep year cid country rank e_`z'_*
		order year cid country rank e_`z'_*
		
		keep if inlist(cid,"TWN","MYS","KOR","VNM","CHL","RUS","IDN","AUS") ///
			  | inlist(cid,"IND","JPN","CAN","DEU","ITA","GBR","USA","FRA") ///
			  | inlist(cid,"BRA","ESP","MEX","PHL","CHN")
		drop if cid=="`cty'"
		keep year rank`yr' country e_*
		reshape wide e*, i(country) j(year)
		sort rank`yr' 
		drop rank`yr'
		order country e_`z'_ndura* e_`z'_dura* e_`z'_ucon* e_`z'_svc* e_`z'_all*
		*gen est2011=1.03*e_`z'_all2010 if "`cty'"=="CHN"
		format e* %9.2f
		
		dataout, save(FD_elasticity_iid3d_`cty'_`z'.tex) dec(2) tex replace

		}
	}
