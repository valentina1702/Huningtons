
//import 
import excel "C:\Users\Mary Grace\Dropbox\01_JHUSOM\Grad_school\Y1_courses\EN.580.631_Comp_Med_Imaging\ICM_Project\HD_ICM_random200.xlsx", sheet("Sheet1") firstrow case(lower) clear

des,f
count
tab diag

//vars 
keep subject_id gender age diagnosis caud* caudate_tail* nucaccumbens* gp* put*

gen hd = 1 if diagnosis=="'HD'"
replace hd =0 if diagnosis=="'NORMAL'"
lab define hd 0 "No HD" 1 "HD"
lab values hd hd

gen male = 0 if gender =="'F'"
replace male = 1 if gender=="'M'"

table1 ,by(hd) ///
vars(male bin \ age contn \ caud_l contn \caud_l conts)

//calculate mean, sd, zscore
//caud_r caudate_tail_l caudate_tail_r nucaccumbens_r nucaccumbens_l gp_l gp_r put_r put_l 

foreach v in caud_l caud_r caudate_tail_l caudate_tail_r nucaccumbens_r nucaccumbens_l gp_l gp_r put_r put_l {
	egen mean_`v' = mean(`v') if hd ==0
	egen sd_`v' = sd(`v') if hd==0
	gsort hd
	replace mean_`v' = mean_`v'[_n-1] if missing(mean_`v')
	replace sd_`v' = sd_`v'[_n-1] if missing(sd_`v')
	gen zscore_`v' = (`v'- mean_`v')/sd_`v'
}	



foreach v in caud_l caud_r caudate_tail_l caudate_tail_r nucaccumbens_r nucaccumbens_l gp_l gp_r put_r put_l {
	tab hd,sum(`v')
	bys hd: sum `v' , det
	
	hist `v',by(hd,col(1) graphregion(c(white))) percent
	graph save `v'.gph,replace
}

graph combine caud_l.gph caud_r.gph,ycommon xcommon graphregion(c(white))


foreach v in caud caudate_tail nucaccumbens gp put {
	tw scatter `v'_l `v'_r if hd==1 ,mc(red%50)|| scatter `v'_l `v'_r if hd==0 ,mc(blue%50)|| ///
	lowess `v'_l `v'_r if hd==0 || lowess `v'_l `v'_r if hd==1 ,  ///
	legend(order(1 "HD" 2 "No HD" 3 "smoothed curve" 4 "smoothed curve")) ///
	ytitle(Left) xtitle(Right) title(`v')
	graph save LvsR_`v'.gph, replace
	
}
graph combine LvsR_caud.gph LvsR_caudate_tail.gph LvsR_nucaccumbens.gph LvsR_gp.gph LvsR_put.gph


foreach v in caud caudate_tail nucaccumbens gp put {
	tw scatter `v'_r age if hd==1 ,mc(red%50)|| scatter `v'_r age if hd==0,mc(blue%50) || ///
	lowess `v'_r age if hd==1 || lowess `v'_r age if hd==0 , ///
	legend(order(1 "HD" 2 "No HD" 3 "smoothed curve" 4 "smoothed curve")) ///
	ytitle(Right) xtitle(Age) title(`v')
	graph save LvsAge_`v'.gph, replace
	
}


graph combine LvsAge_caud.gph LvsAge_caudate_tail.gph LvsAge_nucaccumbens.gph LvsAge_gp.gph LvsAge_put.gph

foreach v in caud caudate_tail nucaccumbens gp put {
	tw scatter `v'_r age if hd==1 & male==0, mc(%50)|| scatter `v'_r age if hd==1 & male== 1, mc(%50) || ///
	scatter `v'_r age if hd==0 & male==0 , mc(%50)  || scatter `v'_r age if hd==0 & male==1, mc(%50) || ///
	lowess `v'_r age if hd==1 & male==0 , lc(%50)   || lowess `v'_r age if hd==1 & male==1, lc(%50) || ///
	lowess `v'_r age if hd==0 & male==0 , lc(%50)   || lowess `v'_r age if hd==0 & male==1 , lc(%50) ///
	legend(order(1 "HD/Female" 2 "HD/Male" 3 "No HD/Female" 4 "No HD/Male" ///
	5 "HD/Female" 6 "HD/Male" 7 "No HD/Female" 8 "No HD/Male")) ///
	ytitle(Right) xtitle(Age) title(`v')
	graph save LvsAge_`v'_bySex.gph, replace 
	
}

graph display LvsAge_caud_bySex.gph
graph combine LvsAge_caud_bySex.gph LvsAge_caudate_tail_bySex.gph 

graph combine LvsAge_caud_bySex.gph LvsAge_caudate_tail_bySex.gph LvsAge_nucaccumbens_bySex.gph LvsAge_gp_bySex.gph LvsAge_put_bySex.gph,legend(none)

//just the lowess curves
foreach v in caud caudate_tail nucaccumbens gp put {
	tw lowess `v'_r age if hd==1 & male==0 , lc(%50)   || lowess `v'_r age if hd==1 & male==1, lc(%50) || ///
	lowess `v'_r age if hd==0 & male==0 , lc(%50)   || lowess `v'_r age if hd==0 & male==1 , lc(%50) ///
	legend(order(1 "HD/Female" 2 "HD/Male" 3 "No HD/Female" 4 "No HD/Male")) ///
	ytitle(Right) xtitle(Age) title(`v')
	graph save lowess_age_`v'_bySex.gph, replace 
	
}

graph combine lowess_age_caud_bySex.gph lowess_age_caudate_tail_bySex.gph lowess_age_nucaccumbens_bySex.gph lowess_age_gp_bySex.gph lowess_age_put_bySex.gph


foreach v in caud caudate_tail nucaccumbens gp put {
	ttest `v'_l,by(male)
	ttest `v'_r,by(male)
	corr `v'_l `v'_r age,sig
	
}


foreach v in caud caudate_tail nucaccumbens gp put {
	logistic hd age male `v'_l
	logistic hd age male `v'_r
}


logistic hd age male caud_l  gp_l put_l



pwcorr age male caud* nucaccumbens* gp* put*,sig


graph matrix caud* nucaccumbens* gp* put* ,by(hd) half ms(p)