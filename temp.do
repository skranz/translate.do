clear 

*SETTING ENVIRONMENT;
#delimit;
capture log close;

log using "C:\Incentives_Matter_Data\Incentives_FINAL\Log Files\Table9_Post BEF.log", replace;
cd "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\Tests";

set more off;
set mem 400m;
set matsize 500;


******************************************************************************************************************;
**Closed School data;
******************************************************************************************************************;
use "Closed School.dta";
gen close=1 if mon10=="Closed" &  mon11=="Closed";
gen schid=idnum;
keep schid close;
sort schid;
save Temp\tempclose, replace;

******************************************************************************************************************;
**PRE_TEST DATA;
******************************************************************************************************************;
*PRETEST VERBAL;
use Baseline\pre_verbal;
rename total_1_to_4 pre_math_v;
rename total_5_to_11 pre_lang_v;
rename total pre_total_v;
gen pre_writ=0;
keep schid childno pre_math_v pre_lang_v pre_total_v pre_writ;
save Temp\temp1, replace;
clear;

*PRETEST WRIT;
use Baseline\pre_written;
rename total_1_to_4 pre_math_w;
rename total_5_to_11 pre_lang_w;
rename total pre_total_w;

gen pre_writ=1;
keep schid childno pre_math_w pre_lang_w pre_total_w pre_writ;
save Temp\temp2, replace;
clear;

*STACK PRETEST;
use Temp\temp1;
append using Temp\temp2;
sort schid childno;
save Temp\testdata, replace;
clear;

******************************************************************************************************************;
**Post TEST DATA;
******************************************************************************************************************;
use Endline\post_verbal;
for var _all: replace X = 0 if X == -999;
for var _all: replace X = 0 if X == -888;
for var _all: replace X = 0 if X == -777;
for var _all: replace X = 0 if X == .;
for var _all: replace X = 0 if X <0;
egen post_math_v=rsum(ques7 ques8 ques9 ques10);
egen post_lang_v=rsum(ques1 ques2 ques3 ques4 ques5 ques6);
replace post_math_v=10*post_math_v/8;
replace post_lang_v=10*post_lang_v/12;
egen post_total_v=rsum(post_math_v post_lang_v);

rename idnum schid;

keep schid childno post_math_v post_lang_v post_total_v;
replace schid=2492 if schid==2493;
sort schid childno;
save Temp\temp3, replace;
clear;


*postTEST WRIT;
use Endline\post_written;
for var _all: replace X = 0 if X == -999;
for var _all: replace X = 0 if X == -888;
for var _all: replace X = 0 if X == -777;
for var _all: replace X = 0 if X ==  777;
for var _all: replace X = 0 if X == .;
for var _all: replace X = 0 if X <0;

egen post_math_w=rsum(_6a _6b _7a _7b _7c _8a _8b _8c _8d _8e _8f _8g _8h _9a _9b  _9c _9d _9e _9f  _9g  _9h c62 c63 c64 c65);
egen post_lang_w=rsum( _1a _1b _1c _1d _1e _1f _1g _1h _1i _1j _2a _2b _2c _2d _2e _2f _2g _2h _2i _2j _3a _3b _3c _3d _3e _4a _4b _4c _4d _4e _4f _4g _5a _5b _5c _5d _5e);
replace post_math_w=10*post_math_w/45;
replace post_lang_w=10*post_lang_w/55;

egen post_total_w=rsum(post_math_w  post_lang_w);

rename sch_id schid;
rename ch_no_ childno;

gen post_writ=1;
keep schid childno post_math_w post_lang_w post_total_w post_writ;
replace schid=2492 if schid==2493;
sort schid childno;
save Temp\temp4, replace;
clear;

*MERGE POST TEST;
use Temp\temp3;
sort schid childno;
merge schid childno using Temp\temp4;
drop _merge;

******************************************************************************************************************;
**MERGING PRE AND post TEST;
******************************************************************************************************************;
sort schid childno;
merge schid childno using Temp\testdata;
save Temp\testdata, replace;

**FIXING MISTAKES IN THE DATA;
replace pre_math_w=56 if schid==4231 & childno==3;
replace pre_total_v=11 if schid==3511 & childno==17;
replace pre_total_w=34 if schid==4252 & childno==22;

gen block = int(schid/1000);

*MERGING IN TREATMENT STATUS;
drop _merge;
sort schid;
merge schid using treatschool;
gen treat = 0;
replace treat = 1 if _merge == 3;

**RECODING VARIABLES;
for var _all: replace X = . if X == -999;
for var _all: replace X = . if X == -888;
for var _all: replace X = . if X == -777;

**MERGING IN CLOSURE STATUS;
drop _merge;
sort schid;
merge schid using Temp\tempclose;

drop if treat==.;

save Temp\test_new.dta, replace;

******************************************************************************************************************;
**SOME SAMPLE STATS;
******************************************************************************************************************;
**CALC SCORES BY SCHID AND LOOKING AT CLOSURES;
sort schid close treat;
collapse (mean) pre_math_v pre_lang_v pre_total_v pre_math_w pre_lang_w pre_total_w 
post_math_v post_lang_v post_total_v post_math_w post_lang_w post_total_w, by(schid close treat);

clear;

**STUDENT LEVEL DATA;
use Temp\test_new.dta;
**DROPING SCHOOLS WITH NO PRE OR POST DATA THAT WERE BASICALLY NEVER OPEN;
drop if schid==1211;
drop if schid==5332;
drop if schid==5711;
drop if schid==1111;
drop if schid==2111;
drop if schid==5221;
drop if schid==5611;

drop if schid==1113;

drop if schid==5731 & childno==24;

**drop if close==1;
save Temp\test_new.dta, replace;


*LOOKING AT ENTRY AND EXIT;
gen entry=0;
replace entry=1 if (pre_total_v==. & pre_total_w==.) & post_total_v!=. ;

gen missing=0;
replace missing=1 if (pre_total_v!=. | pre_total_w!=.) & post_total_v==. ;

gen stayer=0;
replace stayer=1 if missing==0 & entry==0;

save Temp\test_new, replace;

******************************************************************************************************************;
**ADDING SCORES;
******************************************************************************************************************;
drop if stayer==0;

replace pre_math_w=pre_math_w/5;
replace pre_lang_w=pre_lang_w/5;
replace pre_total_w=pre_total_w/5;

replace pre_math_v=0 if pre_math_v==.;
replace pre_lang_v=0 if pre_lang_v==.;
replace pre_total_v=0 if pre_total_v==.;

replace pre_math_w=0 if pre_math_w==.;
replace pre_lang_w=0 if pre_lang_w==.;
replace pre_total_w=0 if pre_total_w==.;

replace post_math_v=0 if post_math_v==.;
replace post_lang_v=0 if post_lang_v==.;
replace post_total_v=0 if post_total_v==.;

replace post_math_w=0 if post_math_w==.;
replace post_lang_w=0 if post_lang_w==.;
replace post_total_w=0 if post_total_w==.;

gen add_post_math=post_math_v+post_math_w;
gen add_post_lang=post_lang_v+post_lang_w;
gen add_post_total=post_total_v+post_total_w;

******************************************************************************************************************;
**Merge in post-Test Data;
******************************************************************************************************************;
gen post_dummy=1;
drop _merge;

sort schid childno;
merge schid childno using Endline\normalization;

save Temp\data_norm, replace;
******************************************************************************************************************;
**NORMALIZING SCORES BY EVERYONE!;
******************************************************************************************************************;
capture program drop zscore;
keep if post_dummy==1; 

program define zscore;
	syntax, option1(string) option2(string);

sort treat;

egen m1_`option1'=mean(`option2') if treat==0 & `option2'!=.;
egen sd1_`option1'=sd(`option2') if treat==0 & `option2'!=.;

egen m_`option1'=mean(m1_`option1');
egen sd_`option1'=mean(sd1_`option1');

gen z_`option1'=(`option1'-m_`option1')/sd_`option1';

end;
zscore, option1(add_post_math) option2(math_old);
zscore, option1(add_post_lang) option2(lang_old);
zscore, option1(add_post_total) option2(total_old);

replace post_writ=0 if post_writ==.;

sort treat;
by treat: sum post_writ;

/*************************************************************/
**ADDING CONTROL DATA;
/*************************************************************/
**BLOCKS;
drop block;
gen block = int(schid/1000);

**TEACHER TEST SCORES;
drop _merge;
sort schid;
merge schid using BaselineChar.dta;

**drop if stayer==.;

sum score;
sum infra;

**TABLE 9 PANEL B;
regress post_writ treat pre_math_v pre_math_w pre_writ block score infra, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9B_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress z_add_post_math treat pre_math_v pre_math_w pre_writ block score infra, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9B_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_lang treat pre_lang_v pre_lang_w pre_writ block score infra, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9B_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_total treat pre_total_v pre_total_w pre_writ block score infra, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9B_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

drop _merge;
sort schid childno;
save Temp\genderdata, replace;

/*************************************************************/
**ADDING GENDER;
/*************************************************************/
clear;

use "C:\Incentives_Matter_Data\Incentives_FINAL\Raw Data\RandomCheck\Roster\Temp\Roster_CODED.dta";
drop if sex==-999;
collapse (mean) sex, by(schid childno);
sort schid childno;

merge schid childno using Temp\genderdata;
replace sex=1 if sex<1.5 & sex!=.;
replace sex=2 if sex>=1.5 & sex!=.;

**BOYS PANEL F;
preserve;
keep if sex==1;

regress post_writ treat pre_math_v pre_math_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9F_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress z_add_post_math treat pre_math_v pre_math_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9F_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_lang treat pre_lang_v pre_lang_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9F_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_total treat pre_total_v pre_total_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9F_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;

**GIRLS PANEL E;
preserve;
keep if sex==2;

regress post_writ treat pre_math_v pre_math_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9E_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket replace;

regress z_add_post_math treat pre_math_v pre_math_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9E_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_lang treat pre_lang_v pre_lang_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9E_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

regress z_add_post_total treat pre_total_v pre_total_w pre_writ, cluster(schid);
outreg treat using "C:\Incentives_Matter_Data\Incentives_FINAL\Output\Table9E_post.csv", bdec(2) nocons se 3aster rdec(2) comma bracket append;

restore;
