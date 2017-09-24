
clear all
set more off

use D:\KDI\GVC\China\Customs\dtafile\CHN_Import_01-14_RawFinal, clear

collapse(sum) Import, by(year regimes)
gen reg="Processing" if regimes=="CPA" | regimes=="PIM"
replace reg="Others" if regimes=="OTHE"
replace reg="Ordinary" if regimes=="ORDI"

collapse(sum) Import, by(year reg)

bysort year: egen IMSH = pc(Import)
format IMSH %9.1f

#delimit ;
twoway (connected IMSH year if reg=="Ordinary", lpattern(longdash) mlabel(IMSH) mlabp(12) mlabs(small))
       (connected IMSH year if reg=="Processing", lpattern(solid) mlabel(IMSH) mlabp(6) mlabs(small))
	   (connected IMSH year if reg=="Others", lpattern(dash_dot) mlabel(IMSH) mlabp(6) mlabs(small)),
	   legend(size(small) label(1 "老馆公开") label(2 "啊傍公开") label(3 "扁鸥公开") col(3))
	   xlabel(1995(2)2011) xtitle("")
	   ylabel(0(10)60, format(%9.0fc)) ytitle("")
	   ;
delimit cr
