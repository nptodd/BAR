
# get estimates and standard errors
summarize_surv_f = function(SurveyId_mod_a, 
                            
                            subset_a = "T", # all births 
                            
                            resample_type_a = "bootstrap",
                            replicates_a = 200, # for bootstrap
                            
                            datasets_a = NULL,
                            
                            type_a="scam", # estimation procedure 
                            
                            sp_a = NULL, 
                            
                            medians_a = F # also compute medians ?
){
  
  cat("\n", SurveyId_mod_a, ":\n..........subset:", subset_a, "\n")
  
  country_loc = gsub("[[:digit:]]{4}(DHS|WFS)$", "", SurveyId_mod_a )
  
  data_loc <- fread(paste0("../data/final_data/dtb_cntry/", country_loc, ".csv"),
                    nThread = 1)
  
  data_loc <- data_loc[SurveyId_mod == SurveyId_mod_a]
  
  TOR <- data_loc[1, .(SurveyId_mod, SurveyYear, DHS_CountryCode_mod)]
  
  if(!is.null(datasets_a)){
    TOR[datasets_a, on=.(SurveyId_mod), `:=`(Region = Region,
                                             Region_code = Region_code,
                                             Region_col  = Region_col)]
  }
  
  data_loc_for_size <- subset(data_loc, eval(parse(text = subset_a)))
  TOR[, N_used:=data_loc_for_size[,.N]] 
  
  cat("N available = ", TOR[, N_used], "\n")
  ages_missing <- setdiff(1:36, unique(data_loc_for_size[,age_interview]))
  # leave aside age 0.3333 
  cat("Ages 1-36 months absent:", 
      ifelse(length(ages_missing), ages_missing, "none"), "\n")
  rm(data_loc_for_size); rm(ages_missing)
  
  
  if(TOR[, N_used]<50){return(TOR)}
  
  if( data_loc[is.na(still_ameno)|is.na(still_bf_any), .N ] ){
    cat("NAs in still_ameno or still_bf_any. Check this is ok!\n")    }
  
  if( data_loc[is.na(still_bf_exclu), .N ] ){
    cat("NAs in still_bf_exclu. Check this is ok!\n")    }
  
  
  if(data_loc[is.na(psu),.N]){   # case of Syria 1978
    data_svy <- svydesign(ids = ~0,
                          strata  = ~stratum, 
                          weights = ~sample_weight,
                          data    = data_loc)
  }else{
    data_svy <- svydesign(ids     = ~psu, 
                          strata  = ~stratum, 
                          weights = ~sample_weight,
                          data    = data_loc,
                          nest    = TRUE ) }
  
  if(resample_type_a=="bootstrap"){
    data_svyrep <- as.svrepdesign(data_svy, type = "bootstrap", 
                                  replicates=replicates_a)
  } else {
    data_svyrep <- as.svrepdesign(data_svy, type = resample_type_a)
  }
  
  
  RES_ameno    <- RES_bf_any    <- RES_bf_exclu     <- NA_integer_
  RES_ameno_md <- RES_bf_any_md <- RES_bf_exclu_md  <- NA_integer_
  
  WITH_REP <- F
  
  ###########################################################################
  ###########################################################################
  ################################### MEANS #################################  
  
  ############################# AMENORRHEA ############################  
  try(RES_ameno    <- withReplicates(data_svyrep, estimate_f, 
                                     var_a = "still_ameno", 
                                     subset_a=subset_a,
                                     
                                     return.replicates = WITH_REP, 
                                     
                                     type_a = type_a,
                                     sp_a=sp_a,
                                     restrict_result_to_a="mean"))
  
  ########################### BREASTFEEDING ###########################  
  try( RES_bf_any   <- withReplicates(data_svyrep, estimate_f, 
                                      var_a = "still_bf_any", 
                                      subset_a=subset_a,
                                      
                                      return.replicates = WITH_REP, 
                                      
                                      type_a = type_a, 
                                      sp_a=sp_a,
                                      restrict_result_to_a="mean"))
  
  TOR[1, `:=`(A = get_est(RES_ameno, replicates = WITH_REP), 
              A_se = get_se(RES_ameno, replicates = WITH_REP),
              
              BANY = get_est(RES_bf_any, replicates = WITH_REP), 
              BANY_se = get_se(RES_bf_any, replicates = WITH_REP))]
  
  ###################### EXCLUSIVE BREASTFEEDING ######################  
  if(data_loc[!is.na(still_bf_exclu), .N]){
    
    try( RES_bf_exclu <- withReplicates(data_svyrep, estimate_f, 
                                        var_a  = "still_bf_exclu",
                                        subset_a=subset_a,
                                        
                                        return.replicates = WITH_REP, 
                                        
                                        type_a = type_a, 
                                        sp_a=sp_a,
                                        restrict_result_to_a="mean"))
    TOR[1, `:=`(BEX = get_est(RES_bf_exclu, replicates = WITH_REP),
                BEX_se = get_se(RES_bf_exclu, replicates = WITH_REP))]
  }
  
  ###########################################################################
  ###########################################################################
  ################################## MEDIANS ################################  
  
  if(medians_a){
    
    ############################# AMENORRHEA (MED) ############################  
    try( RES_ameno_md    <- withReplicates(data_svyrep, estimate_f, 
                                           var_a  = "still_ameno", 
                                           subset_a=subset_a,
                                           
                                           return.replicates = WITH_REP,
                                           
                                           type_a = type_a, 
                                           sp_a=sp_a,
                                           restrict_result_to_a="median"))
    
    ########################### BREASTFEEDING (MED) ###########################  
    try( RES_bf_any_md   <- withReplicates(data_svyrep, estimate_f, 
                                           var_a  = "still_bf_any",
                                           subset_a=subset_a,
                                           
                                           return.replicates = WITH_REP, 
                                           
                                           type_a = type_a, 
                                           sp_a=sp_a,
                                           restrict_result_to_a="median"))
    
    TOR[1, `:=`(Amd = get_est(RES_ameno_md, replicates = WITH_REP),
                Amd_se = get_se(RES_ameno_md, replicates = WITH_REP),
                BANYmd = get_est(RES_bf_any_md, replicates = WITH_REP), 
                BANYmd_se = get_se(RES_bf_any_md, replicates = WITH_REP))]
    
    ###################### EXCLUSIVE BREASTFEEDING (MED) ######################  
    if(data_loc[!is.na(still_bf_exclu), .N]){
      
      try( RES_bf_exclu_md <- withReplicates(data_svyrep, estimate_f, 
                                             var_a  = "still_bf_exclu", 
                                             subset_a=subset_a,
                                             
                                             return.replicates = WITH_REP, 
                                             
                                             type_a = type_a, 
                                             sp_a=sp_a,
                                             restrict_result_to_a="median"))
      
      TOR[1, `:=`(BEXmd = get_est(RES_bf_exclu_md, replicates = WITH_REP), 
                  BEXmd_se = get_se(RES_bf_exclu_md, replicates = WITH_REP))]
    }
    
  } # end if(medians_a)
  
  
  return(TOR)
}

