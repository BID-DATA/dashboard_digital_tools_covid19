* Preprocessing

* Basics 
global path "C:\Users\lgoye\OneDrive\Documents\GitHub\digital-technologies"
use "${path}\raw\data-clean-conjunta.dta", clear

* Keep variables
#delimit ;
keep PAIS
	 Ponderador
	 
	 /* Basics */
	 P1 P2 P3 P4 P5 P6 							
	 
	 /* M3: Uso de tecnologia */
	 P7 
	 P8_1 P8_2 P8_3 
	 P37 
	 P23A P23B 
	 P24A P24B
	 P25A_1 P25A_2 P25A_3 P25A_4 P25A_5 P25A_6 P25A_7 P25A_8 P25A_9 
     P25B_1 P25B_2 P25B_3 P25B_4 P25B_5 P25B_6 P25B_7 P25B_8 P25B_9
     P26A_1 P26A_2 P26A_3 P26A_4 P26A_5 P26A_6 P26A_7 P26A_8 P26A_9 P26A_10
     P26B_1 P26B_2 P26B_3 P26B_4 P26B_5 P26B_6 P26B_7 P26B_8 P26B_9 P26B_10 
     P27A_1 P27A_2 P27A_3
     P27B_1 P27B_2 P27B_3
     P28A_1 P28A_2 P28A_3 P28A_4
     P28B_1 P28B_2 P28B_3 P28B_4 
    
     /* M4: Confianza y COVID-19 */
     P9 P10 P11_1 P11_2 P11_3 P11_4 P11_5 P11_6
    
     /* M5: Tramites */
     P13 P14 P15 P16 P17 P18 P19 P20 P21 P22
    
     /* M7: Confianza tecnologia y COVID-19 */
     P29 P30 P31 P32 P33 P34 P35 P36_1 P36_2 P36_3
  
     /* M7: Comportamiento */
     P38A P38B P39 P41_1 P41_2
	 P12A_1 P12A_2 P12A_3 P12A_4 P12A_5
	 P12B_1 P12B_2 P12B_3 P12B_4 P12B_5;
#delimit cr

* Preprocessing
	* Sample characteristics
	decode PAIS, gen(COUNTRY)
	replace COUNTRY = ustrto(ustrnormalize(COUNTRY, "nfd"), "ascii", 2)
	
	gen     d_P1 = "18-25" if P1 <= 25 
	replace d_P1 = "25-50" if P1 > 25 & P1 <= 50
	replace d_P1 = "50+"   if P1 > 50
	
	decode P2, gen(d_P2)
	
	gen d_P0 = 1 
	
	* Replace to dummies 
	lab def dummy 0 "No" 1 "Si"
	foreach var in P7 P5 P6 P13 P29 P37 P39 {
		gen 	d_`var' = `var'
		replace d_`var' = 0 if `var' == 2
		replace d_`var' = . if `var' == 99
		lab val d_`var' dummy
	}

	* M3: Uso de tecnologia
	foreach var of varlist P8_* {
		gen 	d_`var' = inlist(`var',1,2)
		replace d_`var' = . if `var' == 99
	}
	
	forval num = 23/24 {
		gen 	d_P`num' = inlist(P`num'A,1,2) | inlist(P`num'B,1,4)
		replace d_P`num' = . if P`num'A == 99 & P`num'B == 99
	}
	
	foreach num in 1 3 4 8 {
		gen 	d_P25_`num' = P25A_`num' == 1 | P25B_`num' == 1
		replace d_P25_`num' = . if P25A_`num' == . & P25B_`num' == .
	}
	
	foreach num in 1 4 8 9 {
		gen 	d_P26_`num' = P26A_`num' == 1 | P26B_`num' == 1
		replace d_P26_`num' = . if P26A_`num' == . & P26B_`num' == .
	}
	
	forval num = 1/3 {
		gen 	d_P27_`num' = inlist(P27A_`num',1,2) | inlist(P27B_`num',3)
		replace d_P27_`num' = . if P27A_`num' == 99 & P27B_`num' == 99
	}
	
	forval num = 1/4 {
		gen 	d_P28_`num' = inlist(P28A_`num',1,2) | inlist(P28B_`num',3)
		replace d_P28_`num' = . if P28A_`num' == 99 & P28B_`num' == 99
	}
	
	 * M4: Confianza y COVID-19
	 gen 	 d_P9_1 = P9 == 1 
	 replace d_P9_1 = . if P9 == 99
	 
	 gen 	 d_P9_2 = P9 == 2
	 replace d_P9_2 = . if P9 == 99
    
     * M5: Tramites
    
     * M7: Confianza tecnologia y COVID-19
  
     * M7: Comportamiento
     gen 	 d_P38_1 = inlist(P38A,4,5)
	 replace d_P38_1 = . if P38A == 99
	 
	 gen 	 d_P38_2 = inlist(P38B,4,5)
	 replace d_P38_2 = . if P38B == 99

* Keep variables 
	keep COUNTRY Ponderador d_*
	rename (d_*) (*)
	
* Export data
	export delimited using "${path}\output\data-dashboard.csv", nolabel replace
