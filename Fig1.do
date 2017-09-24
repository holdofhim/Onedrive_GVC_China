
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use Fig1, clear
#delimit ;
twoway (connected GDP Year, lpattern(shortdash) mlabel(GDP) mlabsize(vsmall)) 
	(connected DomesticFinalDemand Year, mlabel(DomesticFinalDemand) msymbol(D) mlabsize(vsmall)) 
	(connected Investment Year, lpattern(_) mlabel(Investment) msymbol(t) mlabsize(vsmall)) 
	(connected Consumption Year, mlabel(Consumption) msymbol(x) mlabsize(vsmall)), 
	xlabel(1995(2)2011, labsize(*.9)) ylabel(0(3)15, labsize(*.9))
	xtitle("")
	ytitle("Real Growth Rate (%, previous year's price)", size(*.95) margin(small))
	xsize(11) ysize(8)
	legend(order(2 1 4 3) label(2 "Final Expenditure") size(*.95))
	;
#delimit cr
graph export D:\OneDrive\GVC_China\Draft\Fig1.eps, replace
