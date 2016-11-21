
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

#delimit ;
graph bar (asis) gr_va_all,
           over(cid, label(labsize(small))) over(yr, label(labsize(small)))
		   blabel(bar, pos(upper) format(%9.1f) size(vsmall))
		   asyvars legend(size(vsmall) row(1))
		   ylabel("") ytitle("")
		   xsize(7) ysize(5)
		   title("(a) Estimated GDP Growth Rates", size(small));
#delimit cr
tempfile a
graph save `a'.gph, replace

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
		   asyvars legend(size(vsmall) row(1))
		   ylabel("") ytitle("")
		   xsize(7) ysize(5)
   		   title("(b) Real GDP Growth Rates", size(small));
#delimit cr

tempfile b
graph save `b'.gph, replace
graph combine `a'.gph `b'.gph, cols(1) xsize(9) ysize(11)

graph export D:\OneDrive\GVC_China\Draft\Estimate_vs_Real_Growth_comparison.eps, replace
