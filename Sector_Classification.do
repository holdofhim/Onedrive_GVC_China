

clear all
set more off
cd D:\OneDrive\GVC_China\Data

import excel using ICIO_ciid_classification.xlsx, sheet("ICIO_industry") first clear
keep iid_icio ind_en ind_3d ind_dnd
drop in 1
dataout, save(D:\OneDrive\GVC_China\draft\Sector_Classification.tex) dec(2) tex replace


import excel using ICIO_ciid_classification.xlsx, sheet("cty_class") first clear
keep cid 
merge 1:m cid using country-codes
drop if _merge==2
drop _merge

