clear all
set more off

clear all
use13 "C:\KDI\GVC\ICIO\Stata\bak.stunicode\China_import_byBEC_1995-2014.dta"
keep if inlist(Year,1995,2010)
replace Partner="Korea" if Partner=="Rep. of Korea"
replace Partner="Vietnam" if Partner=="Viet Nam"

/* Total China import share by BEC_5d 

keep if inlist(Partner,"Malaysia","Korea","Vietnam","USA","Japan","Germany","World")
collapse(sum) TradeValueUS, by(Year Partner BEC_5d)
bysort Year Partner: egen TOT_TV = total(TradeValueUS)
gen TVSH = TradeValueUS / TOT_TV * 100
egen which = group(BEC_5d)
keep Year Partner TVSH which
reshape wide TVSH, i(Year Partner) j(which)

#delimit ;
graph bar TVSH1 TVSH2 TVSH4 TVSH5 TVSH3, 
	  over(Year, lab(labsize(*.7))) over(Partner, lab(labsize(*.85))) stack
	  legend(label(1 "Half-finished") label(2 "Parts") label(3 "Raw Materials") 
	  label(4 "Capital") label(5 "Consumer") size(*.8) col(5))
	  ylabel(,labsize(*.9))
	  blabel(bar, pos(center) format(%6.1f) size(vsmall))
	  xsize(12) ysize(8)
	  ;
#delimit cr

graph export c:\OneDrive\GVC_China\Draft\CHN_Import_by_BEC.eps, replace
*/


* 2. 

drop if Partner=="World"
egen which = group(BEC_3d)
keep if which==3
collapse(sum) TradeValueUS, by(Year Partner)
bysort Year: egen TOT_TV = total(TradeValueUS)
gen TVSH = TradeValueUS / TOT_TV * 100

keep Year Partner TVSH
keep if inlist(Partner,"Malaysia","Korea","Vietnam","USA","Japan","Germany")
gsort Year -TV

