
clear all
set more off
cd D:\OneDrive\GVC_China\Data

use CHN_FD_1995-2011, clear
destring FD*, replace

gen iid = substr(ciid,-3,.)
collapse (sum) FD*, by(iid)
