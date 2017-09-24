

/*
cd D:\OneDrive\GVC_China\Data
wbopendata, indicator(NY.GDP.MKTP.KD.ZG) long clear 
keep if inrange(year,2009,2011)
rename countrycode cid
keep if inlist(cid,"TWN","MYS","KOR","VNM","CHL","RUS","IDN","AUS") ///
	  | inlist(cid,"IND","JPN","CAN","DEU","ITA","GBR","USA","FRA") ///
	  | inlist(cid,"BRA","ESP","MEX","PHL","CHN")
rename ny_gdp_mktp_kd_zg g_gdp 
append using D:\OneDrive\GVC_China\Data\Real_GDP_Growth_Rates_2009-2011_TWN
replace cid="TWN" if cid==""
replace g_gdp=9.4 if cid=="CHN" & year==2009
keep cid year g_gdp
replace g_gdp = round(g_gdp,.1)
reshape wide g_gdp, i(cid) j(year)
save Real_GDP_Growth_Rates_wide_2009-2011, replace
*/


foreach cty in "World" "USA" "CHN" {

	cd D:\OneDrive\GVC_China\Data
	use FD_Real_Growth_Effect_iid3d_`cty'_2009-2011, clear
	destring *, replace
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

	keep year country cid gr_*
	replace gr_va_all = round(gr_va_all,.1) if "`cty'"=="World"
	reshape wide gr*, i(country) j(year)
	rename gr_va_all* `cty'*
	keep  country cid `cty'2009 `cty'2010 `cty'2011
	order country cid `cty'2009 `cty'2010 `cty'2011
	
	tempfile con_`cty'
	save `con_`cty'', replace
	
	}


joinby country using `con_USA'
joinby country using `con_World'
joinby cid using D:\OneDrive\GVC_China\Data\Real_GDP_Growth_Rates_wide_2009-2011
order country *2009 *2010 *2011

dataout, save(D:\OneDrive\GVC_China\draft\FD_Real_Growth_Effect_iid3d_World.tex) dec(2) tex replace

