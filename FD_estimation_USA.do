
clear all
set more off
cd "D:\KDI\GVC\National Accounts\US"

*** From ICIO Table, 2000, 2005, 2008-2011
* See D:\KDI\GVC\National Accounts\US\USATotalFinalDemand.do


*** From US Use Table, 2005-2014
* Concordance between NAICS & ICIO
import excel using NAICS-ICIO_concordance.xlsx, firstrow
rename NAICSCode NAICS
save "D:\KDI\GVC\ICIO\Stata\NAICS-ICIO_concordance", replace

* Concordance between ICIO & our Classification
import excel using D:\KDI\GVC\ICIO\Excel\ICIO_ciid_classification.xlsx, sheet(iid3d) firstrow clear
rename iid_icio ICIO
rename (ename kname post) (iid_eng iid_kr iid_3d)
save ICIO-iid3d_concordance, replace

joinby ICIO using D:\KDI\GVC\ICIO\Stata\NAICS-ICIO_concordance
contract NAICS Name ICIO iid*
drop _freq
replace iid_kr = trim(iid_kr)
order NAICS Name ICIO iid*
save "D:\KDI\GVC\ICIO\Stata\NAICS-ICIO_concordance", replace


* Aggregate nominal FD by our industry classification
foreach i of numlist 2005(1)2014 {
	
	* Domestic Final Demand (DFD) = All Use - Intermediate Use - Export
	import excel using Use_SupplyUseFramework_1997-2014_Summary.xlsx, sheet(`i') cellrange(A7:CO74) clear
	keep A BU CB CO
	rename (A BU CB CO) (Name mUse Export allUse)
	recode mUse Export allUse (.=0)
	gen DFD`i' = allUse - mUse - Export
	
	* Collapse Industry Unit according to ICIO
	joinby Name using "D:\KDI\GVC\ICIO\Stata\NAICS-ICIO_concordance"
	order NAICS Name ICIO
	collapse (sum) DFD`i', by(iid_3d)
	export excel using "D:\KDI\GVC\ICIO\excel\FD_estimation_USA.xlsx", sheet(DFD`i') firstrow(var) sheetrep
	
	}



*** Price Index Calculation

** Option 1. Use BEA gross output price data

* Gross Output (GO) from BEA
cd "D:\KDI\GVC\National Accounts\US"
import excel using GrossOutput_1997-2014.xlsx, sheet(GO) cellrange(B6:T107) clear first
rename B Name
replace Name = trim(Name)
keep Name GO*
tempfile GO
save `GO'

* Output Price from BEA
import excel using GrossOutput_1997-2014.xlsx, sheet(ChainPriceIndexes) cellrange(B7:T107) clear
rename B Name
replace Name = trim(Name)

loc yr = 1997
foreach x of varlist C-T {
	rename `x' P`yr'
	loc ++yr
	}

joinby Name using `GO'
joinby Name using D:\KDI\GVC\ICIO\Stata\NAICS-ICIO_concordance
forval yr=2006/2014  {
	loc lag = `yr'-1
	bysort iid_3d: egen wt`yr' = pc(GO`lag')
	gen PI`yr' = P`yr'/P`lag'
	gen wPI`yr' = PI`yr'*wt`yr'/100
	}
collapse (sum) wPI*, by(iid_3d)
export excel using "D:\onedrive\GVC_China\data\FD_estimation_USA.xlsx", sheet(Price_Index) firstrow(var) sheetrep


** Option 2. Use BLS PPI data
cd "D:\KDI\GVC\National Accounts\US"
import excel using US_PPI_NAICS-3d_2005-2014.xlsx, sheet(PI_by_ICIO) cellrange(A2:J36) firstrow clear
joinby ICIO using ICIO-iid3d_concordance
collapse PI2006-PI2014, by(iid_3d)
export excel using "D:\onedrive\GVC_China\data\FD_estimation_USA.xlsx", sheet(Price_Index) firstrow(var) sheetrep
