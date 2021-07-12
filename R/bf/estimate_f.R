estimate_f <- function(w, data_a,
                       var_a  = "still_ameno",  
                       type_a = "scam",
                       sp_a =  NULL, # ignored if NULL
                       
                       subset_a="T",
                       restrict_result_to_a=NULL
){
  
  if(!startsWith(var_a,"still_") ){ stop("Check var_a") }
  if(!type_a %in% c("scam", "guide", "trussell")){ stop("Check type_a") }
  
  
  useful_col <- "^(still_|age_|SurveyId_mod|bord|hormonal_contracep_cur|ever_bf|energy)"
  data_loc <- data_a[, .SD, .SDcols = names(data_a) %like% useful_col]
  # add weights 
  data_loc[, w_loc:=w]
  
  # restrict to specified subset
  data_loc <- subset(data_loc, eval(parse(text = subset_a)))
  
  setnames(x = data_loc, 
           old = c("age_interview", var_a), 
           new = c("a", "v")) # just shorter
  
  ### deal with NAs 
  # proportion of NAs, by age
  NA_by_age <- data_loc[, .(prop_na=mean(is.na(v))), keyby= a]
  # which ages are all NAs
  ages_allNA <- NA_by_age[prop_na > 0.9999, a]
  # remove those at ages with NAs
  data_loc <- data_loc[ ! a %in% ages_allNA ]
  data_loc[is.na(v), v:=FALSE ] # other NAs set to FALSE (0)
  
  # if too few cases, return NA.
  # restrict to w_loc>0 because of resampling
  if(data_loc[w_loc>0, .N] < 40){return(NA_real_)}
  # this also handles cases where variables on which are selecting are
  # not defined (e.g. wealth variable for old DHS)
  
  #### Shape-constrained GAMs
  if(type_a =="scam"){
    
    K_a <- dplyr::case_when(var_a=="still_ameno" ~ 13,
                            var_a=="still_bf_any" ~ 11, 
                            var_a=="still_bf_exclu" ~ 16, 
                            TRUE ~ 11)
    
    # For estimation in small groups, where the only individuals present may be
    # left out of resample (w_loc == 0)
    data_loc <- data_loc[w_loc>0]
    
    data_loc[,norm_weight:=w_loc/mean(w_loc)]
    
    curve_surv <- data_loc[,.(S=weighted.mean(v, w_loc)), keyby=.(a)]
    
    SCAM <- NA_real_
    
    SurvCond <- (curve_surv[,min(S)] < 0.10) 
    
    
    if(data_loc[, max(a)]<40 & var_a %in% c("still_bf_any", "still_ameno")){ 
      # add data points to help convergence of spline to 0 if max age 36 months. 
      # Why ? We know still_bf or still_ameno  are very likely to be 0 at 61 mo postpartum
      N_add_data <- min( round(data_loc[, .N]/10), 50)
      additional_data = data.table(a=rep(61,  N_add_data), 
                                   v=rep(0, N_add_data), 
                                   norm_weight = sample(data_loc[,norm_weight], N_add_data, T))
      data_loc = rbindlist(list(data_loc[,.(a, v, norm_weight)], additional_data))
    }
    
    
    if(SurvCond){
      
      try(SCAM <- scam(v ~ s(a, bs="mpd", k = K_a),  
                       data = data_loc, 
                       family = quasibinomial, 
                       weights = norm_weight,
                       sp=sp_a,
                       optimizer = "efs"), 
          silent = T)
      
      condition_spline <- "mpd"
      
      if( all(is.na(SCAM)) ){ # if fitting failed, restrict
        
        limit_weight <- data_loc[, median(norm_weight)]/10
        
        try(SCAM <- scam(v ~ s(a, bs="mpd", k = K_a),  
                         data = data_loc[norm_weight>limit_weight],
                         family = quasibinomial, weights = norm_weight,
                         sp=sp_a,
                         optimizer = "efs"), 
            silent = T)
        condition_spline <- "mpd + weights"
      }
      
    } else{ # !SurvCond : impose supplementary condition 
      
      SCAM <- scam(v ~ s(a, bs="mdcx", k = K_a), 
                   data = data_loc, 
                   family = quasibinomial, weights = norm_weight, 
                   sp=sp_a,
                   optimizer = "efs")
      
      condition_spline <- "mdcx"
    } # end if(SurvCond)
    
    if(! all(is.na(SCAM)) ){
      
      lim_pred <- dplyr::case_when(var_a=="still_ameno" ~ 52, # Bangladesh 1975:very long! 
                                   var_a=="still_bf_any" ~ 54, 
                                   var_a=="still_bf_exclu" ~ 20, 
                                   TRUE ~ NA_real_)
      
      x_pred <- seq(0, lim_pred, by = 0.1)
      
      y_pred <- predict(SCAM, data.frame(a=x_pred), type = 'link')
      
      y_pred <- 1/(1+exp(-y_pred))
      
      result_list <- list(scam = SCAM,  
                          mean = mean(y_pred)*(max(x_pred)-min(x_pred)), 
                          median = x_pred[which.min(abs(y_pred-0.5))],
                          method="Shape constrained GAM",
                          condition = condition_spline)
    } else{
      result_list <- NA_real_
    }
    
  } # end if(type_a =="scam")
  
  #### Guide to DHS statistics methodology
  if(type_a == "guide"){
    
    data_loc[, age_group := floor(a/2)] # same as age_interview_grouped, but
    # as age_interview_grouped is stored as "g0", "g1" etc. to   
    # avoid confusion with real ages, stored in age_interview
    
    data_loc[, loc_val_w := v*w_loc]
    
    table_by_age <- data_loc[,.(num = sum(loc_val_w),
                                denum=sum(w_loc)),
                             keyby=.(age_group) ]
    
    table_by_age[, midpoint:=(age_group*2+0.5)]
    
    table_by_age[age_group==0, midpoint:=0.75]
    
    table_by_age[, `:=`(num_smooth =  as.numeric(stats::filter(num, rep(1/3, 3), 
                                                               sides = 2)),
                        denum_smooth = as.numeric(stats::filter(denum, rep(1/3, 3), 
                                                                sides = 2))) ]
    
    # don't smooth first and last group
    table_by_age[age_group %in% c(min(age_group), max(age_group)),
                 `:=`(num_smooth=num, denum_smooth=denum)]
    
    # compute smooth proportions
    table_by_age[,prop:=num_smooth/denum_smooth]
    
    width <- diff(c(0, table_by_age[,midpoint]))
    result_mean <- sum(width*table_by_age[, prop])
    
    # compute median 
    last_midpoint <- table_by_age[,max(midpoint)]
    last_prop <- table_by_age[midpoint==last_midpoint, prop]
    
    if(last_prop<0.5){ 
      
      if(startsWith(var_a, "still_bf") & "ever_bf" %in% names(data_loc)){
        # if ever_bf not in data_loc, then it is assumed
        # that S0 is not needed (currently: not needed for mean_trussell)
        S0 <- data_loc[a <36, weighted.mean(ever_bf,  w_loc)]
      } else if (startsWith(var_a,"still_ameno")){ 
        S0 <- 1  
      }
      
      table_by_age <- rbindlist(list(data.table(age_group=-1, midpoint=0, prop=S0),
                                     table_by_age), use.names = T, fill = T)
      
      first_blw <- table_by_age[age_group>=0 & prop < 0.50, age_group][1]
      
      mi   <- table_by_age[age_group==first_blw, midpoint] 
      pi   <- table_by_age[age_group==first_blw,     prop] 
      # obvious way to compute mim1 and pim1. 
      # mim1 <- table_by_age[age_group==(first_blw-1), midpoint] 
      # pim1 <- table_by_age[age_group==(first_blw-1), prop] 
      # alternative if previous age group not in table. Take the first that is available
      # if previous age is in table, then solutions are equivalent
      mim1 <- table_by_age[age_group < first_blw][age_group==max(age_group), midpoint] 
      pim1 <- table_by_age[age_group < first_blw][age_group==max(age_group), prop]
      
      result_median <- mim1 + (pim1-0.5)/(pim1-pi)*(mi-mim1)
      
      message = "okay"
      
    } else {  # last_prop > 0.5
      
      age_omega <- 60
      
      prop_esp <- 0.01
      
      lambda <- -log(prop_esp/last_prop)/(age_omega-last_midpoint)
      
      result_median <- last_midpoint + 1/lambda*log(2*last_prop)
      
      message <- "last prop > 50% : exponential decay assumed for median"
    }
    
    result_list <- list(mean=result_mean, 
                        median=result_median, 
                        method="Guide to DHS statistics",
                        message = message)
    
  } # end if(type_a == "guide")
  
  #### Trussell et al. methodology
  if(type_a=="trussell"){
    
    curve_surv <- data_loc[,.(S=weighted.mean(v, w_loc)), keyby=.(a)]
    
    curve_surv[abs(a-1/3) < 1e-4, S:=S/2]
    
    result_mean <- curve_surv[, sum(S)] 
    
    # if lowest 'survival' > 10% (e.g. breastfeeding Bangladesh 1994),
    # add a correction based on the assumption of exponential
    # decay until age 60 months, where it is assumed that proportion breastfed is 1%
    
    SurvCond <- (curve_surv[,min(S)] < 0.10)
    
    message="okay"
    
    if( !SurvCond ){
      
      age_omega <- 60 # age at which survival falls at survival epsilon S_esp
      
      S_esp <- 0.01 # survival epsilon
      
      age_max <- curve_surv[,max(a)] # max age observed
      
      S_max   <- curve_surv[a==age_max, S]
      
      lambda <- -log(S_esp/S_max)/(age_omega-age_max)
      
      # add area under curve
      result_mean = result_mean + S_max/lambda*(1-exp(lambda*(age_max-age_omega)))
      
      message=  "min prop > 10% : exponential decay assumed"
    }
    
    result_list <- list(mean=result_mean, median=NA_real_,
                        method="Trussell et al.",
                        message = message)
    
  } # end if(type_a=="trussell")
  
  if(!is.null(restrict_result_to_a)){ # for resampling in summarize_surv_f
    result_list=result_list[[restrict_result_to_a]]}
  
  return(result_list) 
  
} # end of estimate_f
