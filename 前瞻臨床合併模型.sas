%let dir=D:\project paper data;

proc import datafile = "&dir\project_data.xlsx"
out = compare_data DBMS = xlsx ; getnames = yes;
run;
proc print; run;

data compare_data;
set compare_data;
if Time = 'time1' then t='1';
if Time = 'time2' then t='2';
if Time = 'time3' then t='3';
if Time = 'time4' then t='4';
if VISIT = 6 then VISIT = 6;
if VISIT = 12 then VISIT = 12;
if VISIT = 18 then VISIT = 18;
if VISIT = 24 then VISIT = 24;
run;
proc sort data=compare_data;
by VISIT;
run;

proc print data = compare_data; run;

data compare_data0;
set compare_data;
if treatment = 1 then delete;
run;
proc print; run;
data compare_data1;
set compare_data;
if treatment = 0 then delete;
run;
proc print; run;

/* 畫圖判斷變數對 chagerate 的影響*/
/* DDAFLOC  Eye treatment */
proc sgplot data = compare_data0;
vline VISIT/ response = ChangeRate  stat = median group = EyeType    markers;
run;

proc sgplot data = compare_data1;
vline VISIT/ response = ChangeRate  stat = median group = EyeType    markers;
run;

proc sgplot data = compare_data0;
vline VISIT/ response = ChangeRate  stat = median group = Eye  markers;
run;
proc sgplot data = compare_data1;
vline VISIT/ response = ChangeRate  stat = median group = Eye  markers;
run;

proc sgplot data = compare_data0;
vline VISIT/ response = ChangeRate  stat = median group = DDAFLOC   markers;
run;
proc sgplot data = compare_data1;
vline VISIT/ response = ChangeRate  stat = median group = DDAFLOC   markers;
run;

proc sgplot data = compare_data0;
vline VISIT/ response = ChangeRate  stat = median group = Baseline   markers;
run;
proc sgplot data = compare_data1;
vline VISIT/ response = ChangeRate  stat = median group = Baseline   markers;
run;

proc sgplot data = compare_data;
vline VISIT/ response = ChangeRate  stat = median group = treatment   markers;
run;
/*proc means data = compare_data  MEDIAN ;*/
/*class VISIT DDAFLOC;*/
/*var ChangeRate ;*/
/*run;*/
/*proc means data = compare_data  MEDIAN ;*/
/*class VISIT Baseline;*/
/*var ChangeRate ;*/
/*run;*/



/* 畫圖判斷變數對DAF 的影響*/
proc sgplot data = compare_data;
vline VISIT/ response = DAF  stat = median group = EyeType  markers;
run;
proc sgplot data = compare_data;
vline VISIT/ response = DAF  stat = median group = Eye  markers;
run;
proc sgplot data = compare_data;
vline VISIT/ response = DAF  stat = median group = DDAFLOC   markers;
run;
proc sgplot data = compare_data;
vline VISIT/ response = DAF  stat = median group = Baseline   markers;
run;
proc sgplot data = compare_data;
vline VISIT/ response = DAF  stat = median group = treatment   markers;
run;

/* linear or quadratic trend*/
data compare_data_trend;
set compare_data;
t_continious=VISIT;
t_continious_=input(t_continious, best12.);
t_continious_sq= VISIT**2;
drop t_continious;
run;
proc print;run;

/* H0: Linear vs Ha: Quadratic*/
/* G^2 = */

/* Model II Intercept + beta2 Group + beta3 Time + beta4 Group*Time */
/* 適合*/
proc mixed data = compare_data_trend;
class VISIT Eye EyeType DDAFLOC  Baseline  Treatment SIDCD / ref = first;
model ChangeRate = t_continious_    DDAFLOC*t_continious_   Baseline*t_continious_   Treatment*t_continious_   EyeType*t_continious_ ;
/* 假設 t 線性*/
repeated VISIT/ subject = SIDCD type = cs;
random   VISIT /  type = cs subject = SIDCD;
/*
 y^hat = 17.37 - 1.03G + 0.48 t + 0.3 G*t
*/
run;	
/*  model2 : 適用*/
/* 針對重複測量使用*/
/* 等新資料看圖篩變數*/


/* assume covariance matrix = CS*/
/* saturate model  => time discrete*/

/*un*/
proc mixed data = compare_data1 METHOD=MIVQUE0;
class  VISIT Eye EyeType DDAFLOC  Baseline  Treatment SIDCD ;
model ChangeRate =  Baseline DDAFLOC Treatment  VISIT Treatment*VISIT ;
repeated VISIT/ type = un subject = SIDCD;
/* Type: Structure for covariance 
   CS: Compound symmtry structue */
run;
/*cs*/
proc mixed data = compare_data1 METHOD=MIVQUE0 ;
class  VISIT Eye EyeType DDAFLOC  Baseline  Treatment SIDCD ;
model ChangeRate =  Baseline DDAFLOC Treatment  VISIT Treatment*VISIT ;
repeated VISIT/ type = cs subject = SIDCD;
/* Type: Structure for covariance 
   CS: Compound symmtry structue */
run;




/* ancova*/
proc glm data = compare_data;
class  DDAFLOC  Baseline  Treatment  VISIT;
model ChangeRate =   DDAFLOC  Baseline  Treatment  VISIT   Treatment*VISIT;
run;






/* 不適合*/
proc mixed data = compare_data_trend;
class VISIT Eye EyeType DDAFLOC  Baseline  Treatment SIDCD  / ref = first;
model ChangeRate = t_continious_  DDAFLOC*t_continious_     DDAFLOC*t_continious_sq  Treatment*t_continious_     EyeType*t_continious_      Treatment*t_continious_sq     EyeType*t_continious_sq   DDAFLOC*t_continious_   Baseline*t_continious_   DDAFLOC*t_continious_sq   Baseline*t_continious_sq;
repeated VISIT/ subject = SIDCD type = cs;
run;



/* 隨機效果*/
data clinical_data;
set compare_data;
if treatment=0 then delete;
VISIT_= input(VISIT, best12.); 
drop VISIT;
run;
data pros_data;
set compare_data;
if treatment=1 then delete;
VISIT_= input(VISIT, best12.); 
drop VISIT;
run;
proc sort data= clinical_data;
by VISIT_  ;
run;
proc sort data=  pros_data;
by VISIT_  ;
run;

/* 可看出每個人相異的趨勢, 適合隨機效果*/
proc sgplot data = clinical_data;
series x = VISIT_  y = ChangeRate / group=SIDCD markers;
run;
proc sgplot data=pros_data;
loess x = VISIT_   y = ChangeRate / group=SIDCD;
run;



/* 可看出每個人相異的趨勢, 適合隨機效果*/
proc sgplot data =pros_data;
series x = VISIT_  y = ChangeRate / group=SIDCD markers;
run;
proc sgplot data= pros_data;
loess x = VISIT_   y = ChangeRate / group=SIDCD;
run;


/* random effect model*/
proc sort data=  compare_data ;
by t  ;
run;
proc mixed data=compare_data  covtest;
class SIDCD;
model  ChangeRate = t / solution;
random intercept t / subject = SIDCD type = un;
run;

