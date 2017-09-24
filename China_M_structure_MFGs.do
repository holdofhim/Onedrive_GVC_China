clear all
set more off
cd D:\onedrive\GVC_china\data

foreach sector in "C17T19TEX" "C24CHM" "C303233CEQ" {
	import excel using China_M_structure, firstrow
	destring year CHN*, replace
	keep if inlist(year,1995,2000,2005,2010)
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
	
	graph bar (asis) s, over(cnum) over(sector, label(labsize(vsmall))) ///
	over(year, label(labsize(small))) asyvars stack legend(size(small) row(1)) ///
	ytick(0(10)100)
	
	graph export D:\onedrive\GVC_china\draft\M_structure_`sector'.eps, replace

	clear
}

