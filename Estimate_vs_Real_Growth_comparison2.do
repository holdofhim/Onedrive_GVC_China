
clear all
set more off
cd D:\Onedrive\GVC_china\data

loc cid "KOR CHN DEU JPN USA TWN VNM MYS" 
foreach c of loc cid  {
	import excel using FD_Real_Growth_Effect_iid3d_World_2009-2011.xlsx, cellrange(A2) sheet(`c') first clear
	keep if ciid=="`c'_TOT"
	gen yr = substr(year,6,4)
	destring yr gr_va_all, replace
	keep yr gr_va_all
	gen cid="`c'"
	tempfile `c'
	save ``c''
	}

clear 	
foreach c of loc cid {
	append using ``c'' 	
	}
rename (yr gr_va_all) (year g_gdp_est)
joinby year cid using Real_GDP_Growth_Rates_2009-2011

reshape long g_gdp_, i(year cid) j(est_real) s

forval i=2009/2011 {
preserve
keep if year==`i'
#delimit ;
graph bar  g_gdp_,
           over(est_real, relabel(1 "Estimated Growth Rates" 2 "Actual Growth Rates")) 
		   over(cid, label(labsize(medsmall)))
		   blabel(bar, pos(upper) format(%9.1f) size(small))
		   asyvars legend(size(medsmall) row(1))
		   ylabel("") ytitle("")
		   xsize(10) ysize(5);
#delimit cr
*graph export D:\onedrive\GVC_china\draft\GDP_Growth_est_vs_actual_`i'.eps, replace
restore
}

/*
wbopendata, indicator(NY.GDP.MKTP.KD.ZG) long clear 
keep if inrange(year,2009,2011)
keep if inlist(countrycode,"KOR","CHN","DEU","JPN","VNM","USA","MYS") 
rename ny_gdp_mktp_kd_zg g_gdp
append using Real_GDP_Growth_Rates_2009-2011_TWN
replace g_gdp=9.4 if countrycode=="CHN" & year==2009
save Real_GDP_Growth_Rates_2009-2011, replace
*/

use Real_GDP_Growth_Rates_2009-2011, clear
#delimit ;
graph bar (asis) g_gdp,
           over(countrycode, label(labsize(small))) over(year, label(labsize(small)))
		   blabel(bar, pos(upper) format(%9.1f) size(vsmall))
		   asyvars legend(size(small) row(1))
		   ylabel("") ytitle("")
		   xsize(10) ysize(6);
#delimit cr
*graph export D:\OneDrive\GVC_China\Draft\Real_GDP_Growth.eps, replace
