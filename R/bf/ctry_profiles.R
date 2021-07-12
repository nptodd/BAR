# background points

bkgd_pts_no_restrict_l = list(bf_any   = dtb[, .( mean = weighted.mean(still_bf_any, sample_weight)),
                                             keyby=.(SurveyId_mod, age_interview)  ],
                              
                              ameno    = dtb[, .( mean = weighted.mean(still_ameno, sample_weight)),
                                             keyby=.(SurveyId_mod, age_interview)  ],
                              
                              bf_exclu = dtb[, .( mean = weighted.mean(still_bf_exclu, sample_weight)),
                                             keyby=.(SurveyId_mod, age_interview)  ] )

bkgd_pts_no_hc_l = list(bf_any   = dtb[hormonal_contracep_cur==F, 
                                       .( mean = weighted.mean(still_bf_any, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ],
                        
                        ameno    = dtb[hormonal_contracep_cur==F,
                                       .( mean = weighted.mean(still_ameno, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ],
                        
                        bf_exclu = dtb[hormonal_contracep_cur==F,
                                       .( mean = weighted.mean(still_bf_exclu, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ] )

# test
# par(mfrow = c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
# X <- ctry_prof_f("TD", bkgd_pts_a=bkgd_pts_no_hc_l, to_file_a = F, 
#                     add_leg1 = F, add_leg2 = F, add_leg3 = F)
# X
# Y <- ctry_prof_f("TD", bkgd_pts_a=bkgd_pts_no_hc_l, to_file_a = F, type="guide",
#                  add_leg1 = F, add_leg2 = F, add_leg3 = F)
# Y

################################################################################################
################################################################################################
############################## SYSTEMATIC GRAPHS AND COMPUTATIONS ##############################

ncores = 6
CL = makeCluster(ncores)
invisible(clusterEvalQ(CL, {library(data.table); library(survey); library(scam)}))
clusterExport(CL, c("plot_ctry_f", "estimate_f", "BPF_f", "datasets") )


############## 1
dir.create("../figs/profiles_no_restric/", showWarnings = F)
ESTIM_NO_RESTRIC <- parLapply(cl = CL, 
                              X = dtb[,unique(DHS_CountryCode_mod)], 
                              fun = ctry_prof_f,
                              bkgd_pts_a = bkgd_pts_no_restrict_l,
                              directory_a = "profiles_no_restric")
ESTIM_NO_RESTRIC <- rbindlist(ESTIM_NO_RESTRIC)
ESTIM_NO_RESTRIC[, residence:="all"]


############## 2
dir.create("../figs/profiles_no_hc/", showWarnings = F)
ESTIM_NO_HC<- parLapply(cl = CL, 
                        X = dtb[,unique(DHS_CountryCode_mod)], 
                        fun = ctry_prof_f, 
                        bkgd_pts_a = bkgd_pts_no_hc_l,
                        
                        subset_a = "hormonal_contracep_cur==F",
                        
                        directory_a = "profiles_no_hc")
ESTIM_NO_HC <- rbindlist(ESTIM_NO_HC)
ESTIM_NO_HC[, residence:="all"]


## NB : we subsequently take bkgd_pts_no_hc_l as bkgd pts for sensitivity analyses

############## 3
dir.create("../figs/profiles_no_hc_rural", showWarnings = F)
ESTIM_NO_HC_RURAL <- parLapply(cl = CL, 
                               X = dtb[,unique(DHS_CountryCode_mod)], 
                               fun = ctry_prof_f, 
                               bkgd_pts_a = bkgd_pts_no_hc_l,
                               
                               subset_a = "hormonal_contracep_cur==F & residence=='rural'",
                               
                               directory_a = "profiles_no_hc_rural")
ESTIM_NO_HC_RURAL <- rbindlist(ESTIM_NO_HC_RURAL)

dir.create("../figs/profiles_no_hc_urban", showWarnings = F)
ESTIM_NO_HC_URBAN <- parLapply(cl = CL, 
                               X = dtb[,unique(DHS_CountryCode_mod)], 
                               fun = ctry_prof_f, 
                               bkgd_pts_a = bkgd_pts_no_hc_l,
                               
                               subset_a = "hormonal_contracep_cur==F & residence=='urban'",
                               
                               directory_a = "profiles_no_hc_urban")
ESTIM_NO_HC_URBAN <- rbindlist(ESTIM_NO_HC_URBAN)

ESTIM_NO_HC_RURAL[,residence:="rural"]
ESTIM_NO_HC_URBAN[,residence:="urban"]
ESTIM_NO_HC_RURALURBAN = rbindlist(list(ESTIM_NO_HC_RURAL, ESTIM_NO_HC_URBAN))
rm(list = c('ESTIM_NO_HC_RURAL', 'ESTIM_NO_HC_URBAN'))


############## 4
dir.create("../figs/profiles_no_hc_rk1/", showWarnings = F)
ESTIM_NO_HC_rk1 <- parLapply(cl = CL, 
                             X = dtb[,unique(DHS_CountryCode_mod)], 
                             fun = ctry_prof_f, 
                             bkgd_pts_a = bkgd_pts_no_hc_l,
                             
                             subset_a = "hormonal_contracep_cur==F & bord==1",
                             
                             directory_a = "profiles_no_hc_rk1")
ESTIM_NO_HC_rk1 <- rbindlist(ESTIM_NO_HC_rk1)
ESTIM_NO_HC_rk1[, residence:="all"]


############## 5
dir.create("../figs/profiles_no_hc_mab2030_rk23/", showWarnings = F)
ESTIM_NO_HC_MAB2030_rk23 <- parLapply(cl = CL, 
                                    X = dtb[,unique(DHS_CountryCode_mod)], 
                                    fun = ctry_prof_f, 
                                    bkgd_pts_a = bkgd_pts_no_hc_l,
                                    
                                    subset_a = "hormonal_contracep_cur==F & 
                                    bord %in% c(2,3) & mab %in% 20:30",
                                    
                                    directory_a = "profiles_no_hc_mab2030_rk23")
ESTIM_NO_HC_MAB2030_rk23 <- rbindlist(ESTIM_NO_HC_MAB2030_rk23)
ESTIM_NO_HC_MAB2030_rk23[, residence:="all"]


############## 6
dir.create("../figs/profiles_no_hc_rk4p/", showWarnings = F)
ESTIM_NO_HC_rk4p <- parLapply(cl = CL, 
                            X = dtb[,unique(DHS_CountryCode_mod)], 
                            fun = ctry_prof_f, 
                            bkgd_pts_a = bkgd_pts_no_hc_l,
                            
                            subset_a = "hormonal_contracep_cur==F & 
                                    bord >3",
                            
                            directory_a = "profiles_no_hc_rk4p")
ESTIM_NO_HC_rk4p <- rbindlist(ESTIM_NO_HC_rk4p)
ESTIM_NO_HC_rk4p[, residence:="all"]


############## 7
dir.create("../figs/profiles_no_hc_trussell/", showWarnings = F)
ESTIM_NO_HC_TRUSSELL<- parLapply(cl = CL, 
                        X = dtb[,unique(DHS_CountryCode_mod)], 
                        fun = ctry_prof_f, 
                        bkgd_pts_a = bkgd_pts_no_hc_l,
                        
                        subset_a = "hormonal_contracep_cur==F",
                        
                        type_a = "trussell",
                        
                        directory_a = "profiles_no_hc_trussell")
ESTIM_NO_HC_TRUSSELL <- rbindlist(ESTIM_NO_HC_TRUSSELL)
ESTIM_NO_HC_TRUSSELL[, residence:="all"]



stopCluster(CL)

fwrite(ESTIM_NO_RESTRIC, file = "../results/ESTIM_NO_RESTRIC.csv")
fwrite(ESTIM_NO_HC, file = "../results/ESTIM_NO_HC.csv")
fwrite(ESTIM_NO_HC_RURALURBAN, file = "../results/ESTIM_NO_HC_RURALURBAN.csv")
fwrite(ESTIM_NO_HC_rk1, file = "../results/ESTIM_NO_HC_rk1.csv")
fwrite(ESTIM_NO_HC_MAB2030_rk23, file = "../results/ESTIM_NO_HC_MAB2030_rk23.csv")
fwrite(ESTIM_NO_HC_rk4p, file = "../results/ESTIM_NO_HC_rk4p.csv")
fwrite(ESTIM_NO_HC_TRUSSELL, file = "../results/ESTIM_NO_HC_TRUSSELL.csv")


rm(bkgd_pts_no_restrict_l)
rm(bkgd_pts_no_hc_l)


beepr::beep(3)
