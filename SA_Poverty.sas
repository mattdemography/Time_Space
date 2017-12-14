options nofmterr;
options helpbrowser=sas;
ods graphics on;
ods listing close;

data city; set home.ncd_2000;
*if statecd="48";
if fips='48029';
run;

data city2; set city;
*Create Westside;
west=0;
if tracta=171300 then west=1;
if tracta=170600 then west=1; if tracta=170401 then west=1;
if tracta=170700 then west=1; if tracta=170101 then west=1;
if tracta=110700 then west=1; if tracta=171200 then west=1;
if tracta=170800 then west=1; if tracta=170402 then west=1;
if tracta=170102 then west=1; if tracta=110600 then west=1;
if tracta=171500 then west=1; if tracta=171100 then west=1;
if tracta=171000 then west=1; if tracta=170900 then west=1;
if tracta=170300 then west=1; if tracta=170200 then west=1;
if tracta=110500 then west=1; if tracta=160701 then west=1;
if tracta=160600 then west=1; if tracta=160500 then west=1;
if tracta=160100 then west=1; if tracta=160200 then west=1;

*Create Eastside;
east=0;
if tracta=111000 then east=1; if tracta=130700 then east=1;
if tracta=110200 then east=1; if tracta=130100 then east=1;
if tracta=130200 then east=1; if tracta=130600 then east=1;
if tracta=130500 then east=1; if tracta=130300 then east=1;
if tracta=130400 then east=1; if tracta=130700 then east=1;
if tracta=110300 then east=1; if tracta=140100 then east=1;

*All Other;
census=0;
if tracta=171300 then census=1;
if tracta=170600 then census=1; if tracta=170401 then census=1;
if tracta=170700 then census=1; if tracta=170101 then census=1;
if tracta=110700 then census=1; if tracta=171200 then census=1;
if tracta=170800 then census=1; if tracta=170402 then census=1;
if tracta=170102 then census=1; if tracta=110600 then census=1;
if tracta=171500 then census=1; if tracta=171100 then census=1;
if tracta=171000 then census=1; if tracta=170900 then census=1;
if tracta=170300 then census=1; if tracta=170200 then census=1;
if tracta=110500 then census=1; if tracta=160701 then census=1;
if tracta=160600 then census=1; if tracta=160500 then census=1;
if tracta=160100 then census=1; if tracta=160200 then census=1;
if tracta=111000 then census=2; if tracta=130700 then census=2;
if tracta=110200 then census=2; if tracta=130100 then census=2;
if tracta=130200 then census=2; if tracta=130600 then census=2;
if tracta=130500 then census=2; if tracta=130300 then census=2;
if tracta=130400 then census=2; if tracta=130700 then census=2;
if tracta=110300 then census=2; if tracta=140100 then census=2;

*All Other Label;
census1='All Other';
if west=1 then census1='Westside';
if east=1 then census1='Eastside';

*Creating proportions for variables;
	*Income;
		*(Average Income Per Household);
		*1970;
		ainchouse70=avhhin7;
		*1980;
		ainchouse80=avhhin8;
		*1990;
		ainchouse90=avhhin9;
		*2000;
		hhinc00=gmy001;
		ainchouse00=hhinc00;
				
		*(Proportion Public Assistance Income);
		*1970;
		ppubinc70=welfare7;
		*1980;
		ppubinc80=welfare8;
		*1990;
		ppubinc90=welfare9;
		*2000;
		ppubinc00=(h10aaaa001/(h10aaaa001+h10aaaa002));

*Children;
		*1970;			
			malec70=(men47+men97+men147+men177a);
			femalec70=(fem47+fem97+fem147+fem177a);
			total70=(men47+men97+men147+men177a+men197a+ men247 + MEN297 + MEN347 + MEN447 + MEN547 + MEN647 + men747+
			men757 + fem47+fem97+fem147+fem177a+fem197a+ fem247 + FEM297 + FEM347 + FEM447 + FEM547 + FEM647 + fem747+fem757);
			child70=malec70+femalec70;
			pchild70=(child70/total70);
		*1980;
			malec80=(men48+men98+men148+men178a);
			femalec80=(fem48+fem98+fem148+fem178a);
			total80=(men48+men98+men148+men178a+men198a+ men248 + MEN298 + MEN348 + MEN448 + MEN548 + MEN648 + men748+
			men758 + fem48+fem98+fem148+fem178a+fem198a+ fem248 + FEM298 + FEM348 + FEM448 + FEM548 + FEM648 + fem748+fem758);
			child80=malec80+femalec80;
			pchild80=(child80/total80);

		*1990;
			malec90=(men49+men99+men149+men179a);
			femalec90=(fem49+fem99+fem149+fem179a);
			total90=(men49+men99+men149+men179a+men199a+ men249 + MEN299 + MEN349 + MEN449 + MEN549 + MEN649 + men749+
			men759 + fem49+fem99+fem149+fem179a+fem199a+ fem249 + FEM299 + FEM349 + FEM449 + FEM549 + FEM649 + fem749+fem759);
			child90=malec90+femalec90;
			pchild90=(child90/total90);


		*2000;
			malec00=sum (of fmz001-fmz004);
			femalec00=sum (of fmz024-fmz027);
			child00=malec00+femalec00;
			pchild00=(child00/fl5001);

	*Race;
		*1970;
		phis70=shrhsp7;
		*1980;
		phis80=shrhsp8;
		*1990;
		phis90=shrhsp9;
		*2000;
		phis00=(fmc001/(fmc001+fmc002));

	*Family Structure(Female-Headed with Children);
		*1970;
		pfhh70=ffh7;
		*1980;
		pfhh80=ffh8;
		*1990;
		pfhh90=ffh9;
		*2000 (Check to make sure calculations are correct);
		pfhh00=(fnz003/fq8001);

	*Employment 
		(Unemployed);
		*1970;
		punemp70=unemprt7;
		*1980;
		punemp80=unemprt8;
		*1990;
		punemp90=unemprt9;
		*2000;
		empuniverse=(glr001+glr002+glr003+glr004); 
		unemp00=glr002+glr004;
		punemp00=unemp00/empuniverse;

	*Employed;
		*1970;
			*Working Age Population;
			workingm70=(MEN247 + MEN297 + MEN347 + MEN447 + MEN547 + MEN647);
			workingf70=(FEM247 + FEM297 + FEM347 + FEM447 + FEM547 + FEM647);
			working70=workingm70+workingf70;
		pemp70=empmt7/working70;
		*1980;
			*Working Age Population;
			workingm80=(MEN248 + MEN298 + MEN348 + MEN448 + MEN548 + MEN648);
			workingf80=(FEM248 + FEM298 + FEM348 + FEM448 + FEM548 + FEM648);
			working80=workingm80+workingf80;
		pemp80=indemp8/working80;
		*1990;
			*Working Age Population;
			workingm90=(MEN249 + MEN299 + MEN349 + MEN449 + MEN549 + MEN649);
			workingf90=(FEM249 + FEM299 + FEM349 + FEM449 + FEM549 + FEM649);
			working90=workingm90+workingf90;	
		pemp90=indemp9/working90;
		*2000;
			*Working Age Population;
			workingm00=sum(of FMZ006-FMZ017);
			workingf00=sum(of FMZ029-FMZ040);
			working00=workingm00 + workingf00;
		emp00=glr001+glr003;
		pemp00=emp00/working00;

	*Poverty;
		*1970;
		ppov70=povrat7;
		*1980;
		ppov80=povrat8;
		*1990;
		ppov90=povrat9;
		*2000;
		ppov00=(gn6001/gn5001);

*Creating proportion codes for dropouts;
	*1970;
	dropout70=educ87+educ117;
	pdrop70=dropout70/educpp7;
	*1980;
	dropout80=educ88+educ118;
	pdrop80=dropout80/educpp8;
	*1990;
	dropout90=educ89+educ119;
	pdrop90=drops90/educpp9;
	*2000;
	maled00=sum(of GKT001-GKT008);
	femaled00=sum(of GKT017-GKT024);	
	dropout00=(maled00+femaled00);
	pdrop00=(dropout00/educpp0);


*2000 Specific;

	*Isolation;
	langtot00=sum(of gi6001-gi6007);
	isotot00=(gi7001+gi7003+gi7005+gi7007);
	piso00=(isotot00/langtot00);

	*Transportation;
		*Car;
		allcar00=(nocar0 + car10 + car20 + car30);
		pnocar00=(nocar0/allcar00);

		*Commute;
		allcommut00=(commut20+commut40+commutx0);
		plongdrive00=(commutx0/allcommut00);
		pshortdrive=(commut20/allcomut00);

		*Public Assistance;
		tothouse00=h10aaaa001+h10aaaa002;
		percapwelfare=h19aaaa001/tothouse00;


keep gisjoin STATE COUNTY FIPS trctcd west east census census1
ainchouse70 ainchouse80 ainchouse90 ainchouse00
ppubinc70 ppubinc80 ppubinc90 ppubinc00
phis70 phis80 phis90 phis00
pchild70 pchild80 pchild90 pchild00
pfhh70 pfhh80 pfhh90 pfhh00
pemp70 pemp80 pemp90 pemp00
punemp70 punemp80 punemp90 punemp00
ppov70 ppov80 ppov90 ppov00
povrat7n povrat7d
povrat8n povrat8d
povrat9n povrat9d
gn6001 gn5001
pdrop70 pdrop80 pdrop90 pdrop00
metro1973 metro1983 metro1993 metro2003
piso00 pnocar00 plongdrive00 hhinc00 percapwelfare;
run;

*For Work with GIS;
PROC EXPORT DATA= WORK.city2
            OUTFILE= "C:\Users\Matt\Desktop\city2.csv" 
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

proc standard mean=0 std=1 data=city2 out=city2a; var ainchouse70 ainchouse80 ainchouse90 ainchouse00
ppubinc70 ppubinc80 ppubinc90 ppubinc00
phis70 phis80 phis90 phis00
pchild70 pchild80 pchild90 pchild00
pemp70 pemp80 pemp90 pemp00
punemp70 punemp80 punemp90 punemp00
pfhh70 pfhh80 pfhh90 pfhh00
pdrop70 pdrop80 pdrop90 pdrop00; run;


*Create tract-period data set;
data city3; set city2a; 

array ainchouse [1:4] ainchouse70 ainchouse80 ainchouse90 ainchouse00;
array apubinc [1:4] ppubinc70 ppubinc80 ppubinc90 ppubinc00;
array apchild [1:4] pchild70 pchild80 pchild90 pchild00;
array aphisp [1:4] phis70 phis80 phis90 phis00;
array apemp [1:4] pemp70 pemp80 pemp90 pemp00;
array aunemp [1:4] punemp70 punemp80 punemp90 punemp00;
array appov [1:4] ppov70 ppov80 ppov90 ppov00;
array apfhh [1:4] pfhh70 pfhh80 pfhh90 pfhh00;
array apdrop [1:4] pdrop70 pdrop80 pdrop90 pdrop00;
array ametro [1:4] metro1973 metro1983 metro1993 metro2003;
do year=1 to 4;
inchouse=ainchouse[year];
ppubinc=apubinc[year];
pchild=apchild[year];
phisp=aphisp[year];
propemp=apemp[year];
propunemp=aunemp[year];
ppov=appov[year];
pfhh=apfhh[year];
pdrop=apdrop[year];
metro=ametro[year];
time=year-1;
output;
end;


if inchouse ne .;
if ppubinc ne .;
if pchild ne .;
if propemp ne .;
if phisp ne .;
if pfhh ne .;
if pdrop ne .;
if ppov ne .;
if propunemp ne .;

run;


*Create tract-period data set lagging for predictors;
data city3; set city2a; 

array ainchouse [1:3] ainchouse70 ainchouse80 ainchouse90;
array apubinc [1:3] ppubinc70 ppubinc80 ppubinc90;
array apchild [1:3] pchild70 pchild80 pchild90;
array aphisp [1:3] phis70 phis80 phis90;
array apemp [1:3] pemp70 pemp80 pemp90;
array aunemp [1:3] punemp70 punemp80 punemp90;
array appov [1:3] ppov80 ppov90 ppov00;
array apfhh [1:3] pfhh70 pfhh80 pfhh90;
array apdrop [1:3] pdrop70 pdrop80 pdrop90;
array ametro [1:3] metro1973 metro1983 metro1993;
do year=1 to 3;
inchouse=ainchouse[year];
ppubinc=apubinc[year];
pchild=apchild[year];
phisp=aphisp[year];
propemp=apemp[year];
propunemp=aunemp[year];
ppov=appov[year];
pfhh=apfhh[year];
pdrop=apdrop[year];
metro=ametro[year];
time=year-1;
output;
end;


if inchouse ne .;
if ppubinc ne .;
if pchild ne .;
if propemp ne .;
if phisp ne .;
if pfhh ne .;
if pdrop ne .;
if ppov ne .;
if propunemp ne .;

run;

*Graphics;
ods output PearsonCorr = pcorr;
proc corr data=city3 nosimple noprob;
var time;
with west phisp pfhh;
run;

data city4; set city3;
*Testing assumption of high concentration of poverty/underclass neighborhoods;
hpov=0;
if ppov>0.40 then hpov=1;
mpov=0;
if ppov>0.30 then mpov=1;
lpov=0;
if ppov>0.20 then lpov=1;

if trctcd ne 160800;

run;

*Intra-class correlation - Amount of variation between census tracts;
proc mixed data=city4 covtest noclprint method=ml; class trctcd;
model ppov=/solution;
random intercept/subject=trctcd;
run;

*Intra-class correlation - Amount of variation between time.  Tells a lot about neighborhood stability;
proc mixed data=city4 covtest noclprint method=ml; class time;
model ppov=/solution;
random intercept/subject=time;
run;

*Checks for model assumptions;
proc mixed data=city4 covtest method=ml;
class trctcd;
model ppov=time ppubinc propemp pfhh pdrop phisp/outp=testa solution chisq;
random intercept time/type=un sub=trctcd solution;
ods output SolutionR=testa1;
run;

	*Different Residuals - Normality;
	proc univariate data=testa noprint;
	var resid; qqplot/vref=0;
	run;

	proc univariate data=testa1 noprint;
	where effect="Intercept"; *Case Matters;
	var estimate; qqplot/vref=0 normal;
	run;

	proc univariate data=testa1 noprint;
	where effect="time"; *Case Matters;
	var estimate; qqplot/vref=0;
	run;

	*Independence (Homoscedasticity);
	proc sgplot data = testa ;
 	xaxis min = 0 max = 3;
	yaxis min = -2 max = 2;
  	scatter x =  y = resid /markerattrs = (symbol=circlefilled);
  	refline 0 /axis=y;
	run;
	quit;

*Effect of time on Poverty;
proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=/solution notest;
random intercept/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time/solution notest;
random intercept time/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp/solution notest;
random intercept time/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp /solution notest;
random intercept time/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop/solution notest;
random intercept time/ subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop west east/solution notest;
random intercept time west east / subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=reml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop west east
pdrop*west
pdrop*east/solution notest;
random intercept time west east  / subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop west east 
ppubinc*west propemp*west pfhh*west pdrop*west phisp*west 
ppubinc*east propemp*east pfhh*east pdrop*east phisp*east/solution notest;
random intercept time / subject=trctcd type=un;
run;


*Interaction Effect with Dropout Rate and East/West side;
*Create tract-period data set;
data inter; set city2; 

array ainchouse [1:4] ainchouse70 ainchouse80 ainchouse90 ainchouse00;
array apubinc [1:4] ppubinc70 ppubinc80 ppubinc90 ppubinc00;
array apchild [1:4] pchild70 pchild80 pchild90 pchild00;
array aphisp [1:4] phis70 phis80 phis90 phis00;
array apemp [1:4] pemp70 pemp80 pemp90 pemp00;
array aunemp [1:4] punemp70 punemp80 punemp90 punemp00;
array appov [1:4] ppov70 ppov80 ppov90 ppov00;
array apfhh [1:4] pfhh70 pfhh80 pfhh90 pfhh00;
array apdrop [1:4] pdrop70 pdrop80 pdrop90 pdrop00;
array ametro [1:4] metro1973 metro1983 metro1993 metro2003;
do year=1 to 4;
inchouse=ainchouse[year];
ppubinc=apubinc[year];
pchild=apchild[year];
phisp=aphisp[year];
propemp=apemp[year];
propunemp=aunemp[year];
ppov=appov[year];
pfhh=apfhh[year];
pdrop=apdrop[year];
metro=ametro[year];
time=year-1;
output;
end;


if inchouse ne .;
if ppubinc ne .;
if pchild ne .;
if propemp ne .;
if phisp ne .;
if pfhh ne .;
if pdrop ne .;
if ppov ne .;
if propunemp ne .;

run;

proc mixed data=inter covtest noclprint noitprint noinfo method=ml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop west east 
pdrop*west
pdrop*east/solution notest;
random intercept time west east pdrop*west pdrop*east/ subject=trctcd type=un;
run;

*************Interaction Graph*************;
title;
proc format;
	value plot 1 = "Interaction";
run;
data predata;
do i = 0 to 1;
	pdrop=i;
	west=0;
output;
end;
	do i = 0 to 1;
	pdrop=i;
	west=1;
output;
end;
run;
proc mixed data=newnew covtest noclprint noitprint noinfo method=reml; class trctcd;
model ppov=time ppubinc pfhh pchild phisp propemp pdrop west east
pdrop*west/solution notest;
random intercept time west east pdrop*west / subject=trctcd type=un;
store out=cplm;
run;
proc plm source=cplm;
score data=predata out=cplot;
run;
data cplot;
set cplot;
plot=1; run;
proc sgplot data=cplot;
series x=pdrop y=predicted/ group=west; run;


*Plot 1970 Poverty by 2000 Poverty;
ods html style=BarrettsBlue;
proc sgscatter data=city2;
plot ppov00*ppov70/group=census1 grid legend=(Title='Census Tract')
reg=(nogroup);
label ppov00='2000 Proportion Poverty' ppov70='1970 Proportion Poverty';
Title"Persistence and Change in Poverty In San Antonio, Texas";
title2"R=0.657, p<0.001";
run; quit;

proc corr data=city2; var ppov00 ppov70; run;

*Plot 1970 Dropout Rate by 2000 Dropout Rate;
ods html style=BarrettsBlue;
proc sgscatter data=city2;
plot pdrop00*pdrop70/group=census1 grid legend=(Title='Census Tract');
label pdrop00='% 2000 Dropout Rate' pdrop70='% 1970 Dropout Rate';
run; quit;

*Plot 1970 Dropout Rate by 2000 Dropout Rate - Standard Dev.;
proc standard mean=0 std=1 data=city2a out=new; var ppov70 ppov80 ppov90 ppov00; run;

ods html style=BarrettsBlue;
proc sgscatter data=new;
plot ppov00*ppov70/group=census1 grid legend=(Title='Census Tract')
reg=(nogroup);
label ppov00='2000 Standardized Poverty Rate' ppov70='1970 Standardized Poverty Rate';
Title"Persistence and Change in Poverty In San Antonio, Texas";
title2"R=0.657, p<0.001";
run; quit;

*Plot 1970 Dropout Rate by 2000 Dropout Rate - Standard Dev.;
ods html style=education;
proc sgscatter data=new;
plot pdrop00*pdrop70/group=census1 grid legend=(Title='Census Tract');
label pdrop00='2000 Standardized Dropout Rate' pdrop70='1970 Standardized Dropout Rate';
Title"Standardized Dropout Rates In Bexar County, Texas";
title2"From 1970 to 2000";
run; quit;

***************************************************************************;

*Make new data sets for west and east side to calculate means;
data east; set city2;
if east=1;
run;

data west; set city2;
if west=1;
run;

data all; set city2;
if east ne 1;
if west ne 1;

run;

proc means data=east; var ppov70 ppov80 ppov90 ppov00 ppubinc70 ppubinc80 ppubinc90 ppubinc00 
pemp70 pemp80 pemp90 pemp00 pfhh70 pfhh80 pfhh90 pfhh00 
pdrop70 pdrop80 pdrop90 pdrop00 
phis70 phis80 phis90 phis00 pchild70 pchild80 pchild90 pchild00; run;

proc means data=west; var ppov70 ppov80 ppov90 ppov00 ppubinc70 ppubinc80 ppubinc90 ppubinc00 
pemp70 pemp80 pemp90 pemp00 pfhh70 pfhh80 pfhh90 pfhh00 
pdrop70 pdrop80 pdrop90 pdrop00 
phis70 phis80 phis90 phis00 pchild70 pchild80 pchild90 pchild00; run;

proc means data=all; var ppov70 ppov80 ppov90 ppov00 ppubinc70 ppubinc80 ppubinc90 ppubinc00 
pemp70 pemp80 pemp90 pemp00 pfhh70 pfhh80 pfhh90 pfhh00 
pdrop70 pdrop80 pdrop90 pdrop00 
phis70 phis80 phis90 phis00 pchild70 pchild80 pchild90 pchild00; run;

*Calculate the Effect of West and East side on initial poverty and rate of change over time
(Singer Page 69;

proc mixed data=city4 covtest noclprint method=ml; class trctcd;
model ppov=time west time*west/solution notest;
random intercept time/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint method=ml; class trctcd;
model ppov=time east time*east/solution notest;
random intercept time/subject=trctcd type=un;
run;

proc mixed data=city4 covtest noclprint method=ml; class trctcd;
model ppov=time west east time*west time*east/solution notest;
random intercept time/subject=trctcd type=un;
run;


proc freq data=city2;tables west;run;
proc ttest data=city2; var  punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00 ppov00 hhinc00; class west; run;
proc univariate data=city2; var percapwelfare logpov00 ppov00 hhinc00 loghhinc00 gkg001 h19aaaa001; histogram; run;
/*Initial Models;
proc reg data=city2; model loghhinc00=punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00/stb; run;quit;
proc reg data=city2; model loghhinc00=punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00 west/stb vif; run;quit;
proc reg data=city2; model loghhinc00=punemp00 percapwelfare pnocar00 plongdrive00 pdrop00 piso00 psingmom00 west/stb vif; run;quit;

data city3; set city2;
intun=punemp00*west; 
intcar=pnocar00*west; 
intdrive=plongdrive00*west;
intdrop=pdrop00*west;
intiso=piso00*west;
intsing=psingmom00*west;
run;
proc reg data=city3; model loghhinc00=punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00 west
intun intcar intdrive intdrop intiso intsing/stb; run; quit;
proc reg data=city3; model loghhinc00=punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00 west
intun/stb; run; quit;

proc glm data=city3; model loghhinc00=punemp00 pnocar00 plongdrive00 pdrop00 piso00 psingmom00 west punemp00*west; run;quit;
