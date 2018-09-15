sort  iso year

****** Create ccode and first sort *********************************************

cap egen ccode = group(iso)
cap sort ccode year
cap tsset ccode year, yearly

****** Data transformations **************************************************

gen lrgdp   = log(rgdppc)		// real GDP index from Barro
gen lrcon   = 100*log(rconpc)	// real consumption index from Barro
gen lmoney  = log(money)			// M2, more or less
gen lstocks = log(stocks)			// Stock indices
gen lnarrow = log(narrowm)			// M1, more or less
gen lcpi    = log(cpi)				// CPI
gen cay		= ca/gdp				// Current Account over GDP ratio
gen lpop    = log(pop)
gen lloans = log(tloans)
gen lrloans = lloans - lcpi 

rename debtgdp pubgdp 
gen lpubgdp = log(pubgdp) - lcpi    			    // real public debt:gdp					
replace lpubgdp = 100*lpubgdp
replace pubgdp = 100* pubgdp
gen dpubgdp = d.pubgdp

gen egdp = expenditure/gdp 
gen legdp = log(egdp) - lcpi  
replace legdp = 100*egdp
replace egdp = 100* egdp
gen degdp = d.egdp

gen rprv  = lloans - lcpi - lpop 					// real per capita private loans
gen rpub = log(pubgdp) + log(gdp) - lcpi - lpop		// real per capita public debt
gen rtot = rpub + rprv

*** Alan called this variable "loansgdp" for 10Oct2012 version - I am switching to prvgdp here
gen prvgdp = tloans/gdp
gen totgdp = pubgdp + prvgdp

gen riy = iy*rgdppc				// real per capita investment
gen lriy = 100*log(riy)
gen rlmoney = lmoney - lcpi
gen rlnarrow = lnarrow - lcpi

gen dltrate=d.ltrate
gen dstir=d.stir

gen dlrgdp  = 100*d.lrgdp		// Annual real per capita GDP growth in percent
gen dlriy	= d.lriy			// Annual real per capita investment growth in percent
gen dlcpi   = 100*d.lcpi			// Annual inflation in percent
gen dlrcon  = d.lrcon			// Annual real consumption growth in percent

gen drprv = 100*d.rprv				// Annual real per capita private loan growth
gen drpub = 100*d.rpub				// Annual real per capita public debt growth  
gen drlmoney= 100*d.rlmoney 		// Annual Growth in M2 ??100??
gen drlnarrow = 100*d.rlnarrow

replace cay = 100*cay

* construct lags of the  RHS variable set
gen ldrprv		=  l.drprv
gen ldrpub		=  l.drpub
gen ldlrgdp		=  l.dlrgdp
gen ldlcpi		=  l.dlcpi
gen ldlriy		=  l.dlriy
gen lstir		=  l.stir
gen lltrate		=  l.ltrate
gen lcay		=  l.cay

*** REAL ESTATE DATA 
cap rename treal tmort								// total mortgage loans
cap rename hhreal hhmort							// household mortgages
gen tnmort = tloans - tmort						// total non-mortgage loans 


******** Sample housekeeping with the data  ********************************************

gen war=0
replace war = 1 if year>=1914 & year<=1919
replace war = 1 if year>=1939 & year<=1947

forvalues i=1/5 {
	gen f`i'war = f`i'.war
	if `i' <= 2 {
	gen l`i'war = l`i'.war
	}
}

* clean up
capture drop ccode*
egen ccode = group(iso)
sort ccode year
tsset ccode year, yearly

* HP defns, normalized to 1990
gen hpreal = hpnom/cpi
forvalues i =1/17 {
	sum hpreal if year==1990 & ccode==`i'
	replace hpreal = hpreal/r(mean) if ccode==`i'
	}
gen lhpreal = log(hpreal)

forvalues i =1/17 {
	sum hpnom if year==1990 & ccode==`i'
	replace hpnom = hpnom/r(mean) if ccode==`i'
	}
gen lhpnom = log(hpnom)

gen lhpy = lhpreal - lrgdp
forvalues i =1/17 {
	sum lhpy if year==1990 & ccode==`i'
	replace lhpy = lhpy-r(mean) if ccode==`i'
	}

replace lhpy = 100*lhpy
replace lhpreal = 100*lhpreal
gen dlhpy = d.lhpy 
gen dlhpreal = d.lhpreal

gen spnom = stocks

forvalues i =1/17 {
	sum spnom if year==1990 & ccode==`i'
	replace spnom = spnom/r(mean) if ccode==`i'
	}
gen lspnom = log(spnom)

capture drop stocksreal
gen stocksreal = spnom/cpi

forvalues i =1/17 {
	sum stocksreal if year==1990 & ccode==`i'
	replace stocksreal = stocksreal/r(mean) if ccode==`i'
	}

*
capture drop lspreal
gen lspreal = log(stocksreal)

gen lspy = lspreal - lrgdp
forvalues i =1/17 {
	sum lspy if year==1990 & ccode==`i'
	replace lspy = lspy-r(mean) if ccode==`i'
	}

replace lspreal = 100*lspreal
gen dlspreal = d.lspreal

replace lcpi = 100*lcpi 
replace lrgdp = 100*lrgdp

* sort and tsset then exit
sort ccode year
tsset ccode year, yearly

gen loansgdp = tloans/gdp
gen mortgdp = tmort/gdp
gen nmortgdp = tnmort/gdp 

replace loansgdp = 100*loansgdp
replace mortgdp = 100*mortgdp
replace nmortgdp = 100*nmortgdp

gen dloansgdp = d.loansgdp
gen dmortgdp=d.mortgdp
gen dnmortgdp=d.nmortgdp

