

clear all
set more off
cd C:\OneDrive\GVC_China\Data

use 10SD_jan15, clear
rename *, lower
keep if inlist(variable,"VA_Q05")
gen mfg_sh = man/sum*100
gen VAS_MFG =(min+man+pu+con)/sum*100

merge 1:1 country year using c:\KDI\Project\2016\China_Focus\Fig4, update replace
keep if inlist(country,"CHN","FRA","JPN","GBR","USA","TWN") | ///
	    inlist(country,"KOR","ESP","DEW","NLD","SGP","MYS")
keep if inrange(year,1980,2015)
keep country year *sh VAS_MFG logGDPpc

* Graphs
#d ;
keep if inlist(country,"CHN","FRA","JPN","GBR","USA","TWN","KOR","MYS") ;
twoway (scatter mfg_sh logGDPpc if country=="KOR", m(dh))
	(scatter mfg_sh logGDPpc if country=="TWN", m(sh))
	(scatter mfg_sh logGDPpc if country=="MYS", m(th))
	(scatter mfg_sh logGDPpc if country=="JPN", m(sh))
	(scatter mfg_sh logGDPpc if country=="FRA", m(X))
	(scatter mfg_sh logGDPpc if country=="GBR", m(oh))
	(scatter mfg_sh logGDPpc if country=="USA", m(th)),
	xlabel(,labsize(*.8)) ylabel(5(5)40,labsize(*.8))
	xtitle("Log of Real GDP per Capita (contant 2011 US dollars)", size(small) margin(small)) 
	ytitle("Manufacturing Sectors' Shares at constant 2005 prices (%)", size(small) margin(small))
	xsize(11) ysize(8)
	legend(order(1 "KOR" 2 "TWN" 3 "MYS" 4 "JPN" 5 "FRA" 6 "UK" 7 "USA") rows(1) size(*.9))
	;
#d cr
*graph export D:\OneDrive\GVC_China\Draft\2nd_industry_shares.eps, replace


/*
#d ;
keep if inlist(country,"CHN","FRA","JPN","GBR","USA","TWN","KOR","MYS") ;
twoway (scatter VAS_MFG logGDPpc if country=="KOR", m(dh))
	(scatter VAS_MFG logGDPpc if country=="CHN", m(X) mc(black))
	(scatter VAS_MFG logGDPpc if country=="FRA", m(oh))
	(scatter VAS_MFG logGDPpc if country=="GBR", m(th))
	(scatter VAS_MFG logGDPpc if country=="JPN", m(sh))
	(scatter VAS_MFG logGDPpc if country=="TWN", m(oh))
	(scatter VAS_MFG logGDPpc if country=="USA", m(th)),
	xlabel(,labsize(*.8)) ylabel(15(5)50,labsize(*.8))
	xtitle("Log of Real GDP per Capita (contant 2011 US dollars)", size(small) margin(small)) 
	ytitle("Secondary Sectors' Shares at constant 2005 prices (%)", size(small) margin(small))
	xsize(11) ysize(8)
	legend(order(1 "Korea" 2 "China" 3 "France" 4 "UK" 5 "Japan" 6 "Taiwan" 7 "USA") rows(1) size(*.9))
	;
#d cr
*graph export D:\OneDrive\GVC_China\Draft\2nd_industry_shares.eps, replace
*/
