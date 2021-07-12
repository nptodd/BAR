Groups_studies <- tribble(~region, ~name, ~G1y, ~G2y, ~G3y, ~G4y,  ~G5y, 
                          "ASI", "BD", 1994, 2000, 2007, 2014, NA_integer_,
                          "ASI", "KH", NA,   2000, 2010, 2014, NA_integer_,
                          "ASI", "ID", 1994, 2003, 2007, 2017, NA_integer_,
                          "ASI", "NP", 1996, 2001, 2011, 2016, NA_integer_,
                          "ASI", "PK", 1991, 2006, 2012, 2017, NA_integer_,
                          "ASI", "PH", 1993, 2003, 2013, 2017, NA_integer_,
                          
                          "ASI","IAw", 1993, 1999, 2006, 2015, NA_integer_,
                          "ASI","IAn",  1993, 1999, 2006, 2015, NA_integer_,
                          "ASI", "IAne", 1993, 1999, 2006, 2015, NA_integer_,
                          "ASI", "IAe",  1993, 1999, 2006, 2015, NA_integer_,
                          "ASI", "IAs", 1993, 1999, 2006, 2015, NA_integer_,
                          "ASI", "IAc", 1993, 1999, 2006, 2015, NA_integer_,
                          
                          "SSA", "BJ", 1996, 2001, 2006, 2012, 2017,  
                          "SSA", "CM", 1991, 1998, 2004, 2011, NA, 
                          "SSA", "ET", NA,   2000, 2005, 2011, 2016, 
                          "SSA", "GH", 1993, 1998, 2003, 2008, 2014, 
                          "SSA", "GN", NA,   1999, 2005, 2012, 2018, 
                          "SSA", "KE", 1993, 1998, 2003, 2008, 2014, 
                          "SSA", "ML", 1996, 2001, 2006, 2012, 2018,
                          "SSA", "MW", 1992, 2000, 2004, 2010, 2015,
                          "SSA", "NG", 1990, 2003, 2008, 2013, 2018, 
                          "SSA", "NI", 1992, 1998, 2006, 2012, NA, 
                          "SSA", "RW", 1992, 2000, 2005, 2010, 2015,
                          "SSA", "SN", 1993, 1997, 2005, 2010, 2015,
                          "SSA", "TZ", 1992, 1999, 2004, 2010, 2015, 
                          "SSA", "UG", 1995, 2000, 2006, 2011, 2016, 
                          "SSA", "ZM", 1992, 1996, 2002, 2007, 2013, 
                          "SSA", "ZW", 1994, 1999, 2005, 2010, 2015,
                          
                          # keep 3 character code for Asia 2
                          "AS2", "BD", 1994, 2000, 2007, 2014, NA_integer_,
                          "AS2", "KH", NA,   2000, 2010, 2014, NA_integer_,
                          "AS2", "ID", 1994, 2003, 2007, 2017, NA_integer_,
                          "AS2", "NP", 1996, 2001, 2011, 2016, NA_integer_,
                          "AS2", "PK", 1991, 2006, 2012, 2017, NA_integer_,
                          "AS2", "PH", 1993, 2003, 2013, 2017, NA_integer_,)


# add SurveyId_mod names for each of the 5 'waves'
Gy_vec <- paste0("G", 1:5, "y")

for(I in 1:5 ){
  
  tempG <-paste0(Groups_studies$name, 
                 Groups_studies[[ Gy_vec[I] ]], 
                 "DHS")
  tempG <- ifelse(tempG %in% datasets[,SurveyId_mod], tempG, NA_character_)
  
  Groups_studies[[paste0("G", I)]] <- tempG
  
}
rm(tempG); rm(Gy_vec)


##### Data for BEX - AMENORRHEA RELATIONSHIP

dir.create("../results/designs_BEX_A")


all_surveys <- c(Groups_studies$G1, Groups_studies$G2, Groups_studies$G3, 
                 Groups_studies$G4, Groups_studies$G5)
all_surveys <- unique(all_surveys[!is.na(all_surveys)])


dtb_BEX <- dtb[SurveyId_mod %in% all_surveys, 
               .(SurveyId_mod, stratum, psu, sample_weight,
                 age_interview, 
                 still_ameno, still_bf_exclu, hormonal_contracep_cur,
                 residence, elec_g, water30less_g, bord, v445) ]

# are weights the same in psu ?
dtb_BEX[, max(sample_weight)-min(sample_weight), 
        by=.(SurveyId_mod, stratum, psu)][,summary(V1==0)]


# makes sure no stratum / PSUs homonyms  
dtb_BEX[, stratum_mod:=paste0(SurveyId_mod, stratum)]
dtb_BEX[, stratumpsu_f:=as.factor(paste0(SurveyId_mod, stratum, psu))]
dtb_BEX[, SurveyId_mod_f:=as.factor(SurveyId_mod)]


fwrite(dtb_BEX, "../results/designs_BEX_A/dtb_BEX.csv")


save_design_f = function(group_def_a, Groups_studies_a=Groups_studies){
  
  region_loc <- substr(group_def_a, 1, 3)
  g_loc <- substr(group_def_a, 5, 6)
  
  surv_dt_loc <- Groups_studies_a %>% filter(region==region_loc) 
  
  surveys_loc <- surv_dt_loc[[g_loc]]
  surveys_loc <- surveys_loc[!is.na(surveys_loc)]
  
  dtb_loc <-  dtb_BEX[SurveyId_mod %in% surveys_loc]
  
  data_svy <- svydesign(ids     = ~psu,
                        strata  = ~stratum_mod,
                        weights = ~sample_weight,
                        data    = dtb_loc,
                        nest    = TRUE )
  
  data_svyrep <- as.svrepdesign(data_svy, type = "bootstrap")
  
  saveRDS(data_svy, paste0("../results/designs_BEX_A/svy", group_def_a, ".rds"))
  
  saveRDS(data_svyrep, paste0("../results/designs_BEX_A/svyrep", group_def_a, ".rds"))
}

Sys.time()
save_design_f("ASI-G1")
save_design_f("ASI-G2")
save_design_f("ASI-G3")
save_design_f("ASI-G4")

save_design_f("SSA-G1")
save_design_f("SSA-G2")
save_design_f("SSA-G3")
save_design_f("SSA-G4")
save_design_f("SSA-G5")

save_design_f("AS2-G1")
save_design_f("AS2-G2")
save_design_f("AS2-G3")
save_design_f("AS2-G4")

rm(save_design_f)
Sys.time()
