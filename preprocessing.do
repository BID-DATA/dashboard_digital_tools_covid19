/*******************************************************************************
Program 	: Dashboard Digital Tools COVID-19
Author		: Laura Goyeneche, consultant
			  Code based on preprocessing by Juan Manuel, consultant
Dependency	: SCL-SPH
Repository 	: dashboard_digital_tools_covid19
*******************************************************************************/

* Basics 
* 10,260 vars x 683 vars
global path "C:\Users\lgoye\OneDrive\Documents\GitHub\dashboard_digital_tools_covid19"
use "${path}\raw\data-clean-conjunta.dta", clear

* Select variables
* 10,260 vars x 123 vars
********************************************************************************
#delimit ;
keep 
	 /* Basics */
	 PAIS
	 Ponderador
	 
	 /* Demographics */
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
     P9 
	 P10 
	 P11_1 P11_2 P11_3 P11_4 P11_5 P11_6
    
     /* M5: Tramites */
     P13 P14 P15 P16 P17 P18 P19 P20 P21 P22
    
     /* M7: Confianza tecnologia y COVID-19 */
     P29 P30 P31 P32 P33 P34 P35 
	 P36_1 P36_2 P36_3
  
     /* M8: Comportamiento */
     P38A P38B 
	 P39A_1 P39A_2 P39A_3 P39A_4 P39A_5 P39A_6
	 P39B_1 P39B_2 P39B_3 P39B_4 P39B_5 P39B_6
	 P41_1 P41_2
	 P12A_1 P12A_2 P12A_3 P12A_4 P12A_5
	 P12B_1 P12B_2 P12B_3 P12B_4 P12B_5;
#delimit cr

* Preprocessing
********************************************************************************
	* Sample characteristics
		* Country name
		decode PAIS, gen(COUNTRY)
		replace COUNTRY = ustrto(ustrnormalize(COUNTRY, "nfd"), "ascii", 2)
		
		* Age ranges
		gen     d_P1 = "18-25" if P1 <= 25 
		replace d_P1 = "25-50" if P1 > 25 & P1 <= 50
		replace d_P1 = "50+"   if P1 > 50
		
		* Gender/Sex
		decode P2, gen(d_P2)
		
		* Temporary variable
		gen d_P0 = 1 
	
	* Replace to dummies 
	lab def dummy 0 "No" 1 "Si"
	foreach var of varlist P5 P6 P7 P13 P16 P19 P20 P29 P36* P37 P41* {
		gen 	d_`var' = `var'
		replace d_`var' = 0 if `var' == 2
		replace d_`var' = . if `var' == 99
	}

	* M3: Uso de tecnologia
		* Actividades realizadas por el celular 
		* Algunos o todos los dias
			foreach var of varlist P8_* {
				* Dummies
				gen 	d_`var' = inlist(`var',1,2)
				replace d_`var' = . if `var' == 99
				
				* Categorical
				decode `var', gen(d_`var'_cat)
			}
		
		* Descagaria un app del gobierno para reportar sintomas
			forval i = 23/24 {
				* Dummies
				gen 	d_P`i' = inlist(P`i'A,1,2) | inlist(P`i'B,1,4)
				replace d_P`i' = . if P`i'A == 99 & P`i'B == 99
				
				* Categorical 
				gen 	d_P`i'_cat = ""
				replace d_P`i'_cat = "Ya instaló" 		 		   if P`i'A == 1 | P`i'B == 1
				replace d_P`i'_cat = "Seguro instalaría" 		   if P`i'A == 2 | P`i'B == 4
				replace d_P`i'_cat = "Probablemente instalaría"    if P`i'A == 3
				replace d_P`i'_cat = "Probablemente desinstalaría" if P`i'B == 3
				replace d_P`i'_cat = "No instalaría" 		  	   if P`i'A == 4 | P`i'B == 2
			}
		
		* Razones para instalar o no una app
			forval i = 1/9 {
				* Instalar/no desinstalar
					* Dummies
					gen 	d_P25_`i' = P25A_`i' == 1 | P25B_`i' == 1
					replace d_P25_`i' = . if P25A_`i' == . & P25B_`i' == .
					
					* Categorical
				
				* No instalar/desinstalar
					* Dummies
					gen 	d_P26_`i' = P26A_`i' == 1 | P26B_`i' == 1
					replace d_P26_`i' = . if P26A_`i' == . & P26B_`i' == .
					
					* Categorical
					
			}
		
		* Razones para instalar la app
			forval i = 1/3 {
				* Dummies
				gen 	d_P27_`i' = inlist(P27A_`i',1,2) | inlist(P27B_`i',3)
				replace d_P27_`i' = . if P27A_`i' == 99 & P27B_`i' == 99
				
				* Categorical
				gen 	d_P27_`i'_cat = ""
				replace d_P27_`i'_cat = "Seguro instalaría" 	   	  if P27A_`i' == 1 | P27B_`i' == 3
				replace d_P27_`i'_cat = "Probablemente instalaría" 	  if P27A_`i' == 2
				replace d_P27_`i'_cat = "Probablemente desinstalaría" if P27B_`i' == 2
				replace d_P27_`i'_cat = "No instalaría" 			  if P27A_`i' == 3 | P27B_`i' == 1
			}
		
		* Aprobacion de acuerdo al disenador de la app
			forval i = 1/4 {
				* Dummies
				gen 	d_P28_`i' = inlist(P28A_`i',1,2) | inlist(P28B_`i',3)
				replace d_P28_`i' = . if P28A_`i' == 99 & P28B_`i' == 99
				
				* Categorical
				gen 	d_P28_`i'_cat = ""
				replace d_P28_`i'_cat = "Seguro instalaría" 	   	  if P28A_`i' == 1 | P28B_`i' == 3
				replace d_P28_`i'_cat = "Probablemente instalaría" 	  if P28A_`i' == 2
				replace d_P28_`i'_cat = "Probablemente desinstalaría" if P28B_`i' == 2
				replace d_P28_`i'_cat = "No instalaría" 			  if P28A_`i' == 3 | P28B_`i' == 1
			}
	
	 * M4: Confianza y COVID-19 
		* Confianza hacia los demas
			* Dummies 
			gen 	 d_P9_1 = P9 == 1 
			replace d_P9_1 = . if P9 == 99
			 
			gen 	 d_P9_2 = P9 == 2
			replace d_P9_2 = . if P9 == 99
			
			* Categorical
			decode  P9, gen(d_P9_cat)
		
		* Confianza en el gobierno
			* Dummy 
			gen 	d_P10_1 = P10 == 1
			replace d_P10_1 = . if P10 == 99 
			
			* Categorical 
			decode  P10, gen(d_P10_cat)
		
		* Confianza a entidades publicas 
			foreach var of varlist P11_* {
				* Dummies
				gen 	d_`var' = inlist(`var',1,2)
				replace d_`var' = . if `var' == 99
				
				* Categorical
				decode `var', gen(d_`var'_cat)
			}
			
     * M5: Tramites
		foreach var of varlist P14 P15 P17 P18 P21 P22 {
			decode `var', gen(d_`var'_cat)
		}
	 	 
     * M7: Confianza tecnologia y COVID-19
		foreach var of varlist P30 P31 P32 P33 P34 P35  {
			decode `var', gen(d_`var'_cat)
		}
		
     * M8: Comportamiento
		* Cumple recomendaciones de autoridades
			foreach var of varlist P38A P38B {
				decode `var', gen(d_`var'_cat)
			}
			
		* Cumplimiento de restricciones
			foreach var of varlist P39* {
				* Dummies
				gen 	d_`var' = inlist(`var',1,2)
				replace d_`var' = . if `var' >= 98
				
				* Categorical
				decode `var', gen(d_`var'_cat)
				replace d_`var'_cat = "" if `var' >= 99
			}
			
		* Restricciones del gobierno
			forval i = 1/5 {
				gen 	d_P12_`i' = (P12A_`i' == 1) | (P12B_`i' == 1)
				replace d_P12_`i' = . if P12A_`i' == 99 & P12B_`i' == 99
			}

	* Keep variables 
	* 10,260 vars x 114 vars
		keep COUNTRY Ponderador d_*
		rename (d_*) (*)
	
* Export data
********************************************************************************

export delimited using "${path}\output\data-dashboard.csv", nolabel replace

********************************************************************************
