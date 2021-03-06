
clear all
set more off
cd D:\Onedrive\GVC_china\data

loc cid "KOR CHN DEU JPN USA TWN MEX MYS CAN" 
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

#delimit ;
graph bar (asis) gr_va_all,
           over(cid, label(labsize(small))) over(yr, label(labsize(medsmall)))
		   blabel(bar, pos(upper) format(%9.1f) size(small))
		   asyvars legend(size(small) row(1))
		   ylabel("") ytitle("")
		   xsize(12) ysize(7);
#delimit cr
graph export D:\Onedrive\GVC_china\draft\Estimated_GDP_Growth.eps, replace

/*
wbopendata, indicator(NY.GDP.MKTP.KD.ZG) long clear 
keep if inrange(year,2009,2011)
keep if inlist(countrycode,"KOR","CHN","DEU","JPN","MEX","USA","MYS","CAN") 
rename ny_gdp_mktp_kd_zg g_gdp 
append using Real_GDP_Growth_Rates_2009-2011_TWN
replace g_gdp=9.4 if countrycode=="CHN" & year==2009
save Real_GDP_Growth_Rates_2009-2011, replace
*/

use Real_GDP_Growth_Rates_2009-2011, clear
#delimit ;
graph bar (asis) g_gdp,
           over(countrycode, label(labsize(small))) over(year, label(labsize(medsmall)))
		   blabel(bar, pos(upper) format(%9.1f) size(small))
		   asyvars legend(size(small) row(1))
		   ylabel("") ytitle("")
		   xsize(12) ysize(7);
#delimit cr
graph export D:\Onedrive\GVC_china\draft\Real_GDP_Growth.eps, replace
