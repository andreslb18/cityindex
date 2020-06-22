clear all

*******************************
*Do índice segunda clasificación
*

cd "C:\Users\User\Documents\Investigaciones\Terriotrios Inteligentes\Base paper"

set more off

import excel "STATA base.xlsx", firstrow
enc ID, generate(ID2)

******************************
*imputacion datos falatntes

drop c13 // variable de percepción
*variables missing
misstable sum

*eliminar variables con menos del 50% de datos


/*variables a imputar
. misstable sum
                                                               Obs<.
                                                +------------------------------
               |                                | Unique
      Variable |     Obs=.     Obs>.     Obs<.  | values        Min         Max
  -------------+--------------------------------+------------------------------
            a5 |         8                  24  |     24   15.86141    83.30149
           a12 |         1                  31  |     31      .4955    305.8104
           a13 |         8                  24  |     24   1.035554    6.399621
            b6 |        12                  20  |     14   65.42702         100
            b7 |        13                  19  |     19   .0332074    107.9619
            b8 |        19                  13  |     13       25.2       76.41
           b13 |         8                  24  |     24   9.473095    7724.868
           b14 |         8                  24  |     24          0    7678.029
            c3 |         1                  31  |     19         36         100
            c5 |         6                  26  |      8          0          29
            d4 |         8                  24  |     24   19.74491    53.62513
            d6 |         8                  24  |     20         .5        16.5
            d7 |         2                  30  |     30   41.34381    85.86323
           d11 |        10                  22  |     22         12        47.9
           d12 |        10                  22  |     20       .401        .531
            e2 |         1                  31  |     31   8.362469    19.58281
            e3 |         1                  31  |     31   .3113325    6.172094
            e4 |         5                  27  |     27   258.9381    330.5231
            e5 |         5                  27  |     27   279.5648    338.8295
            e6 |         5                  27  |     27    270.885    337.2332
            f1 |         9                  23  |     21          0          10
            i3 |         2                  30  |     30          0      306235
  -----------------------------------------------------------------------------

*/

drop b8 //menos del 50% de variables disponibles

*drop b10

misstable pattern, freq

* Chequear significancia estadstiica de las variables - Matriz de correlaciones
* Muestra las variables estadisicamente significativas al 20% o menos, * las que tienen 10% o menos
set more off
pwcorr a1-n8, print(.2) star(0.1) bonferroni 

* Proceso de on set wide
mi set wide
mi register imputed a5 a12 b13
mi describe


* Fija semilla para el proceso de imputaciiofijar la semilla no permite que cambien los valores aleatorios)
set seed 1234

mi impute mvn a5= f3 j1 j2 j3 j4, add(1) replace
mi impute mvn a12= b1 b4 c16 d3 e13, add(1) replace
mi impute mvn b13=a6 a10 b3 c9 c11 c16, add(1) replace

mi extract 1, clear

* Proceso de on set wide
mi set wide
mi register imputed b14 d7
mi describe

* Fija semilla para el proceso de imputaciiofijar la semilla no permite que cambien los valores aleatorios)
set seed 1234
* Remplazar valores imputados en la base original
mi impute mvn b14=a6 a10 b3 b13 c9 c11 c16, add(1) replace
mi impute mvn d7=n6, add(1) replace 
mi extract 1, clear

mi set wide
mi register imputed c3 d7 d11

* Fija semilla para el proceso de imputaciiofijar la semilla no permite que cambien los valores aleatorios)
set seed 1234
mi impute mvn c3=a12 b11, add(1) replace 
mi impute mvn d7=n6, add(1) replace 
mi impute mvn d11=a12 b1 e14 i4 n3 n6, add(1) replace  
* Remplazar valores imputados en la base original
mi extract 1, clear


mi set wide
mi register imputed e5
set seed 1234

* Remplazar valores imputados en la base original
mi impute mvn e5= d11 e14, add(1) replace

mi extract 1, clear

mi set wide
mi register imputed e4

* Fija semilla para el proceso de imputaciiofijar la semilla no permite que cambien los valores aleatorios)
set seed 1234

* Remplazar valores imputados en la base original
mi impute mvn e4= e5, add(1) replace
mi extract 1, clear

mi set wide
mi register imputed e6 i3

* Fija semilla para el proceso de imputaciiofijar la semilla no permite que cambien los valores aleatorios)
set seed 1234

* Remplazar valores imputados en la base original
mi impute mvn e6= e4 e5, add(1) replace
mi impute mvn i3= a15 j2 j3 j4 n4, add(1) replace
mi extract 1, clear

misstable sum

* Variables no imputadas a13 b6 b7 c5 d4 d6 d12 e2 e3 f1
set more off
* Datos faltantes variables negativas - castigar valores faltantes con el valor del percentil 80 
foreach var of varlist a13 c5 d6 d12{
	pctile pct=`var', nq(10)
	gen pct80=r(r8)
	return list
	replace `var'=pct80 if missing(`var')
	drop pct pct80
}

* Datos faltantes variables positivas - castigar valores faltantes con el valor del percentil 20
foreach var of varlist b6 b7 d4 e2 e3 f1{
	pctile pct=`var', nq(10)
	gen pct20=r(r2)
	return list
	replace `var'=pct20 if missing(`var')
	drop pct pct20
}

********************
**Estandarizacion max-min
*impacto positivo

foreach var of varlist a1 a3-a9 a11 a14-a18 b3-b7 b15 c3-c4 c6-c8 c15 d1-d2 d4 e1-e11 e15-j7 n3-n5 n7-n8{
	egen max=max(`var')
	egen min=min(`var')
	gen x=`var'
	gen stdMax`var'=(x-min)/(max-min)
	drop max min x 
}

*impacto negativo -ojo con b10 en base def d3 negativo pq entre más gande menos diversificado b8 iría acá

foreach var of varlist a2 a10 a12-a13 b1-b2 b9-b14 c1-c2 c5 c9-c14 c16 d3 d5-d7 d10-d12 n1-n2 n6{
	egen max=max(`var')
	egen min=min(`var')
	gen x=`var'
	gen stdMax`var'=(max-x)/(max-min)
	drop max min x 
}

set more off
*********************************
****Definir varibales dimensiones

global xlista stdMaxe1-stdMaxe11 
global xlistb stdMaxh1-stdMaxh2 stdMaxf3
global xlistc stdMaxn8 stdMaxf4-stdMaxf8 stdMaxd10 stdMaxd4 stdMaxb15
global xlistd stdMaxi1-stdMaxi4 stdMaxe15 stdMaxd2 stdMaxd3 stdMaxc15 stdMaxc16
global xliste stdMaxj5 stdMaxj6 stdMaxn1-stdMaxn2 stdMaxn3-stdMaxn7 stdMaxa14 ///
	stdMaxa16 stdMaxa17 stdMaxa6 stdMaxn6
global xlistf stdMaxj1-stdMaxj4 stdMaxj7 stdMaxf1 stdMaxf2
global xlistg stdMaxa15 stdMaxa18 stdMaxc3 stdMaxc4 stdMaxc6-stdMaxc8 ///
	stdMaxc1-stdMaxc2 stdMaxc5 stdMaxc9-stdMaxc14 
global xlisth stdMaxa1 stdMaxa2 stdMaxa3-stdMaxa5 stdMaxa7-stdMaxa9 stdMaxa10 ///
	stdMaxa11 stdMaxa12 stdMaxa13 stdMaxb1-stdMaxb2 stdMaxb3-stdMaxb7 ///
	stdMaxb9 stdMaxb11-stdMaxb14 stdMaxd1 stdMaxd5-stdMaxd7 ///
	stdMaxd11-stdMaxd12

global xlistid ID2

***********************
********** Dimdensión A
describe $xlista
summarize $xlista
corr $xlista

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlista, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc1, score

return list

matrix dimensiona = J(11,2,.)

mat dimensiona = r(scoef)

mat list dimensiona


/*
forvalues i=1/14 {
forvalues j=1/2 {

matrix dimensiona1(`i',`j')=dimensiona(`i'^2,`j'^2)
}
}
*/

mat list dimensiona
scoreplot, mlabel(ID2)

putexcel set dimdensionA.xlsx, replace
putexcel A1 = matrix(dimensiona), rownames


***********************
********** Dimdensión B
describe $xlistb
summarize $xlistb
corr $xlistb

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlistb, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
*scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc2, score

return list

matrix dimensionb = J(3,2,.)

mat dimensionb = r(scoef)

mat list dimensionb

*scoreplot, mlabel(ID2)

putexcel set dimdensionB.xlsx, replace
putexcel A1 = matrix(dimensionb), rownames


***********************
********** Dimdensión C
describe $xlistc
summarize $xlistc
corr $xlistc

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlistc, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc3, score

return list

matrix dimensionc = J(9,2,.)

mat dimensionc = r(scoef)

mat list dimensionc

scoreplot, mlabel(ID2)

putexcel set dimdensionC.xlsx, replace
putexcel A1 = matrix(dimensionc), rownames


***********************
********** Dimdensión D
describe $xlistd
summarize $xlistd
corr $xlistd

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlistd, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc4, score

return list

matrix dimensiond = J(9,2,.)

mat dimensiond = r(scoef)

mat list dimensiond

scoreplot, mlabel(ID2)

putexcel set dimdensionD.xlsx, replace
putexcel A1 = matrix(dimensiond), rownames

***********************
********** Dimdensión E
describe $xliste
summarize $xliste
corr $xliste

* Analisis de componentes principales con el criterio de Kaiser 
pca $xliste, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc5, score

return list

matrix dimensione = J(13,2,.)

mat dimensione = r(scoef)

mat list dimensione

scoreplot, mlabel(ID2)

putexcel set dimdensionE.xlsx, replace
putexcel A1 = matrix(dimensione), rownames


***********************
********** Dimdensión F
describe $xlistf
summarize $xlistf
corr $xlistf

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlistf, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
*screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
*scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc6, score

return list

matrix dimensionf = J(13,2,.)

mat dimensionf = r(scoef)

mat list dimensionf

*scoreplot, mlabel(ID2)

putexcel set dimdensionF.xlsx, replace
putexcel A1 = matrix(dimensionf), rownames

***********************
********** Dimdensión G
describe $xlistg
summarize $xlistg
corr $xlistg

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlistg, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
*screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
*scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc7, score

return list

matrix dimensiong = J(16,2,.)

mat dimensiong = r(scoef)

mat list dimensiong

*scoreplot, mlabel(ID2)

putexcel set dimdensionG.xlsx, replace
putexcel A1 = matrix(dimensiong), rownames

***********************
********** Dimdensión H
describe $xlisth
summarize $xlisth
corr $xlisth

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlisth, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
*screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
*scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc8, score

return list

matrix dimensionh = J(29,2,.)

mat dimensionh = r(scoef)

mat list dimensionh

*scoreplot, mlabel(ID2)

putexcel set dimdensionH.xlsx, replace
putexcel A1 = matrix(dimensionh), rownames

***********************
********** TODAS DIM

global xlisttotal stdMaxa1-stdMaxn6

describe $xlisttotal
summarize $xlisttotal
corr $xlisttotal

* Analisis de componentes principales con el criterio de Kaiser 
pca $xlisttotal, comp(2) mineigen(1) blanks(.3)

* Grafico de sedimentacinese los valores propios
screeplot, yline(1)

* Rotar componentes
rotate, varimax blanks(.3)
rotate, clear

* Graficos de dispersione las cargas y las variables de puntuacionesloadingplot 
scoreplot, mlabel(ID2)

* Cargas en pantalla y puntuaciones de los componentes
* Los valores en pantalla del componente 1 (Comp1) deben elevarse al cuadrado y multiplicarse por 100 para hallar los pesos de cada variable
estat loadings
predict Cpc12, score

return list

*contar número de variables

unab vars : stdMaxa1-stdMaxn6
di `: word count `vars''

matrix dimensiontotal = J(97,2,.)

mat dimensiontotal = r(scoef)

mat list dimensiontotal

scoreplot, mlabel(ID2)

putexcel set dimdensionT.xlsx, replace
putexcel A1 = matrix(dimensiontotal), rownames

export excel city ID stdMaxa1-stdMaxn6 using stdx.xlsx, firstrow(variables) replace




