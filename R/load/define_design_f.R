
define_design_f = function(dtb_a){
  
  dtb_a[, region_urbrur := paste(v101, v102, sep="-")]
  
  dtb_a[, `:=`( motherid = paste(SurveyId_original, caseid, sep="_"),
                
                childid  = paste(SurveyId_original, caseid, bidx, sep="_"),
                
                sample_weight = v005/1e6 ,
                
                psu = v001, 
                
                stratum = region_urbrur)]
  
  if( length(unique(dtb_a[,childid]))!=dtb_a[,.N] ){stop("childid is not unique !!!")}
  
  # if psu absent
  dtb_a[is.na(psu) | psu=="", psu:=v021]
  
  # if region or urban/rural is absent
  surveys_no_strat <- dtb_a[is.na(v101) | v101=="" | is.na(v102) | v102=="",
                            .(.N, N_v022=length(unique(v022)),
                              N_v023=length(unique(v023))), 
                            by=SurveyId_original] 
  
  for(I in 1:nrow(surveys_no_strat)){  dtb_a[SurveyId_original == I, stratum:= v022]  }
  
  # if only one psu in some strata, then may be an implausible stratification
  surveys_bad_psu1 <- unique(dtb_a[, .(N_psu=length(unique(psu))), 
                                   by=.(SurveyId_original, stratum)][N_psu<2, SurveyId_original])
  
  for(I in surveys_bad_psu1){
    
    dtb_a[SurveyId_original == I, stratum:= v022] }
  
  
  # if only one psu in some strata, then stratification still implausible
  surveys_bad_psu2 <- unique(dtb_a[, .(N_psu=length(unique(psu))), 
                                   by=.(SurveyId_original, stratum)][N_psu<2, SurveyId_original])
  
  for(I in surveys_bad_psu2){ 
    
    dtb_a[SurveyId_original == I, stratum:= v023]  }
  
  surveys_bad_psu3 <- unique(dtb_a[, .(N_psu=length(unique(psu))), 
                                   by=.(SurveyId_original, stratum)][N_psu<2, SurveyId_original])
  
  if(length(surveys_bad_psu3)>0){
    cat("Strata with < 2 psu !!!\n")
    for(i in seq_along(surveys_bad_psu3)){ cat(surveys_bad_psu3[i], "\n") }
  }
  
  
  ######### Special known cases 
  
  #### Positive knowledge (from the DHS user forum)
  dtb_a[SurveyId_original == "AZ2006DHS", stratum:= region_urbrur] # Azerbaijan 2006
  dtb_a[SurveyId_original == "TJ2012DHS", stratum:= region_urbrur] # Tajikistan 2012
  
  dtb_a[SurveyId_original == "BU2010DHS", stratum:= v022] # Burundi 2010
  dtb_a[SurveyId_original == "CM2011DHS", stratum:= v022] # Cameroon 2011
  dtb_a[SurveyId_original == "CD2013DHS", stratum:= v022] # Democratic Republic of Congo 2013-14
  dtb_a[SurveyId_original == "RW2015DHS", stratum:= v022] # Rwanda 2014-15
  dtb_a[SurveyId_original == "ZW2015DHS", stratum:= v023] # Zimbabwe 2014-15
  
  #### India
  dtb_a[SurveyId_original == "IA1993DHS", stratum:= paste(v024, v022, sep="-")] 
  dtb_a[SurveyId_original == "IA1999DHS", stratum:= paste(v024, v022, sep="-")] 
  dtb_a[SurveyId_original == "IA2006DHS", stratum:= v022 ] 
  dtb_a[SurveyId_original == "IA2015DHS", stratum:= v023 ] 
  
  
  #### Egypt
  #### "for most surveys of Egypt [...] there are two clusters
  #### per PSU. [...] use v021 as the PSU id"
  dtb_a[DHS_CountryCode_original == "EG", psu:= v021] 
  # for that survey v021 is constantly NA
  dtb_a[SurveyId_original == "EG1988DHS", psu:= v001] 
  #  "For that survey, v021 is incorrect and should be reconstructed"
  dtb_a[SurveyId_original == "EG1995DHS", psu:= as.integer(floor(v001/1e4))] 
  
  
  return(0)
}