clear all
set more off
cd D:\onedrive\GVC_china\data

foreach sector in "212" "214" "222" "223" "224" {

	clear
	import excel using China_M_structure, firstrow
	
	destring year CHN*, replace
	keep if inlist(year,1995,2010)
	drop if cid=="USA"
	keep year cid *_`sector'
	gen obs=_n
	gen cnum=1 if cid=="CHN"
	replace cnum=2 if cid=="JPN"
	replace cnum=3 if cid=="KOR"
	replace cnum=4 if cid=="TWN"
	replace cnum=5 if cid=="ROW"
	label define cnum 1 CHN 2 JPN 3 KOR 4 TWN 5 ROW
	label values cnum cnum
	rename CHNDOM_`sector' s1
	rename CHNPRO_`sector' s2
	rename CHNNPR_`sector' s3
	reshape long s, i(obs) j(which)
	gen sector = "."
	replace sector = "DOM" if which==1
	replace sector = "PRO" if which==2
	replace sector = "NPR" if which==3
	
	if "`sector'"=="212" loc title "Textiles"
	else if "`sector'"=="214" loc title "Petrochemical Products"
	else if "`sector'"=="222" loc title "Computers & Electronic Equipments"
	else if "`sector'"=="223" loc title "Electrical Equipments"
	else if "`sector'"=="224" loc title "Automobiles"
		
	graph bar (asis) s, over(cnum) over(sector, label(labsize(*.85))) ///
	over(year, label(labsize(medsmall))) asyvars stack legend(size(*.85) row(1)) ///
	title(`title', size(*.9) margin(small)) ///
	ytick(0(10)100) xsize(8) ysize(5) ///
	saving(M_structure_`sector', replace)

	}

grc1leg M_structure_212.gph M_structure_214.gph, xsize(12) ysize(6)
graph export D:\onedrive\GVC_china\draft\M_structure_Nondurables.eps, replace
grc1leg M_structure_222.gph M_structure_223.gph, xsize(12) ysize(6)
graph export D:\onedrive\GVC_china\draft\M_structure_Durables.eps, replace
