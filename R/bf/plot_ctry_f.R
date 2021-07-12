plot_ctry_f = function(DHS_CountryCode_mod_a, 
                       var_a="still_ameno", # what variable is investigated (amen, bf, bf_exclu)
                       subset_a="T",
                       
                       type_a="scam",
                       
                       bkgd_pts_a=NULL,     # points to be plotted in the background
                       
                       age_max_a=Inf,       # to check compatibility with published DHS stats
                       eps_a=1e-6,
                       
                       add_info_a="no",
                       add_leg_a=F,
                       detail_leg_a=2,
                       txt_cex_a=1.2,
                       lwd_a=2){
  
  if(!startsWith(var_a, "still_") ){ stop("Incorrect var_a") }
  if(!add_info_a %in% c("no", "reduced", "full") ){ stop("Incorrect add_info_a") }
  
  ##########################################################
  ######################## LOADING  ######################## 
  
  data_path <- paste0("../data/final_data/dtb_cntry/", DHS_CountryCode_mod_a, ".csv") 
  
  data_loc <- fread(data_path, nThread = 1) 
  
  # Table of results
  # make sure consistent colours across surveys even when exclusive bf missing for some
  TOR <- unique(data_loc[,.(SurveyId_mod, SurveyYear, DHS_CountryCode_mod)])
  setkey(TOR, "SurveyYear") # make sure order is ok
  
  TOR[,`:=`(mean=NA_real_, median=NA_real_)]
  
  pal_loc <- wesanderson::wes_palette("Zissou1", TOR[,.N], type = "continuous")
  TOR[, col:= pal_loc]
  
  setnames(data_loc, old = var_a, new = "loc_val")
  
  # restrict to specified subset
  data_loc <- subset(data_loc, eval(parse(text = subset_a)))
  
  # exclude NA values of loc_val. 
  # reason why NAs are present should have been carefully checked!!!
  data_loc <- data_loc[!is.na(loc_val)]
  
  cat("N analyzed (all surveys) for", var_a,":", data_loc[,.N], "\n")
  
  if(is.null(bkgd_pts_a)){ # if arg not supplied, background points assumed in data_loc
    bkgd_pts_a <- data_loc[age_interview < age_max_a,
                           
                           .( mean = weighted.mean(loc_val, sample_weight)),
                           
                           keyby=.(SurveyId_mod, age_interview) ]  }
  
  
  ##########################################################
  ################# MAIN GRAPHICAL FEATURES ################
  
  if(startsWith(var_a, "still_ameno")){
    XLIM_MAX <- 30
    YLAB <- "Proportion of mothers still amenorrheic"
    lim_seg <- 20
  } else if (var_a=="still_bf_any") {   
    XLIM_MAX <- 60
    YLAB <- "Proportion of infants still breastfed"
    lim_seg <- 40
  } else if (var_a=="still_bf_exclu") {
    XLIM_MAX <- 18
    YLAB <- "Proportion of infants still exclusively breastfed"
    lim_seg <- 12
  }
  
  plot(bkgd_pts_a[,.(age_interview, mean)], 
       ylim=c(0,1), xlim=c(0, XLIM_MAX),
       xlab="", ylab= "",
       pch=20, cex=0.5, axes=F, col="grey")
  
  mtext("Time since childbirth (months)", side = 1, line = 4, cex=txt_cex_a)
  mtext(YLAB, side = 2, line = 4, cex=txt_cex_a)
  
  
  ##########################################################
  ################ AGE SPECIFIC PROPORTIONS ################
  
  # age specific proportions
  asp_loc <- data_loc[ age_interview < age_max_a,
                       
                       .( mean = weighted.mean(loc_val, sample_weight)),
                       
                       keyby=.(SurveyId_mod, SurveyYear, 
                               DHS_CountryCode_mod,
                               age_interview) ]
  
  setnames(data_loc, old = "loc_val", new = var_a)
  
  N_samp <- TOR[,.N]
  
  ##########################################################
  ######################## ADD LINES #######################
  
  for(I in 1:N_samp){
    
    SurveyId_mod_loc <-  TOR[I, SurveyId_mod]
    
    lines(asp_loc[SurveyId_mod == SurveyId_mod_loc , .(age_interview, mean)],
          col =  TOR[I, col], lty=2)
    points(asp_loc[SurveyId_mod == SurveyId_mod_loc , .(age_interview, mean)], 
           col =  TOR[I, col], pch=20, cex =1 )
    
    
    data_loc_loc <- data_loc[SurveyId_mod == SurveyId_mod_loc]
    
    # data_loc_loc already restricted to subset_a
    estimate_loc <- estimate_f(w = data_loc_loc[,sample_weight], 
                               data_a =  data_loc_loc, 
                               var_a = var_a, 
                               type_a = type_a)
    
    if(!all(is.na(estimate_loc))){
      TOR[I, mean := estimate_loc$mean]
      TOR[I, median := estimate_loc$median]
      
      if(type_a=="scam"){
        x_pred <- seq(0, 60, 0.1)
        lines(x_pred, predict(estimate_loc$scam, data.frame(a=x_pred), type = 'response'), 
              col =  TOR[I, col], lwd=lwd_a)
        }
    } 
    
  } # end  for(I in 1:N_samp)
  
  
  ##########################################################
  ################ OTHER GRAPHICAL FEATURES ################
  
  segments(0, 0.5, lim_seg, 0.5, lty=2, col="grey30", lwd=2)
  
  if(add_leg_a){
    
    if(detail_leg_a==0) {
      
      leg <- TOR[, SurveyYear]
      
      cex_leg = 2.5
      
    } else if (detail_leg_a==1) {
      
      median_leg <- TOR[, format(round(median, 1), nsmall=1)]
      
      leg <- paste0(TOR[,SurveyYear], " (M=", median_leg, ")")
      
      cex_leg = 2
      
    }else{
      
      mean_leg <- TOR[, format(round(mean, 1), nsmall=1)]
      
      median_leg <- TOR[, format(round(median, 1), nsmall=1)]
      
      leg <- paste0(TOR[,SurveyYear], " (M=", mean_leg, "; m=", median_leg, ")")
      
      cex_leg = 1.7
      
    }
    
    legend("topright", legend = leg, col= TOR[,col], y.intersp = 1.3,
           cex = cex_leg,
           inset = 0.1, pch = 17, pt.cex = txt_cex_a+2.5, bty="n")
  }
  
  axis(1, at=seq(0, XLIM_MAX, 6), cex.axis=2, tck=-0.025, padj = 1)
  axis(1, at=seq(0, XLIM_MAX), labels=rep("", XLIM_MAX+1), tck=-0.015)
  axis(2, at=seq(0, 1, 0.2), las=2, cex.axis=2, tck=0.02)
  axis(2, at=seq(0, 1, 0.1), las=2, labels = rep("", 11), tck=0.02)
  
  TOR[,col:=NULL]
  
  return(TOR)
}

# # test 
# plot_ctry_f("BD", "still_ameno", subset_a = "residence=='rural'",  type_a="scam")
# plot_ctry_f("BD", "still_ameno", subset_a = "residence=='rural'",  type_a="guide")
