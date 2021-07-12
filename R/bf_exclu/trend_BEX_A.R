
trend_A_BEX_f = function(group_def_ext_a="ASI-G1-all",
                         Groups_studies_a=Groups_studies,
                         data_a=dtb_BEX,
                         PRED_TO_AGE_a = 6){
  
  PRED_TO_AGE_a <- 6
  
  region_loc <- substr(group_def_ext_a, 1, 3)
  g_loc <- substr(group_def_ext_a, 5, 6)
  restrict_rural <- ( substr(group_def_ext_a, 8, 10)=="rur")
  
  surv_dt_loc <- Groups_studies_a %>% filter(region==region_loc) 
  
  surveys_loc <- surv_dt_loc[[g_loc]]
  surveys_loc <- surveys_loc[!is.na(surveys_loc)]
  
  dtb_loc <-  data_a[SurveyId_mod %in% surveys_loc]
  
  # load survey objects defined in group_surveys
  data_svy <-  readRDS(paste0('../results/designs_BEX_A/svy', substr(group_def_ext_a, 1, 6), '.rds'))
  data_svyrep <-  readRDS(paste0('../results/designs_BEX_A/svyrep', substr(group_def_ext_a, 1, 6), '.rds'))
  
  #### 1- Design-based, proportions
  
  if(!restrict_rural){
    form_loc <- ~age_interview + still_bf_exclu + hormonal_contracep_cur
  }else{
    form_loc <- ~age_interview + still_bf_exclu + hormonal_contracep_cur + residence
  }
  
  age_spec <- setDT(svyby(~still_ameno, form_loc, data_svy, svymean))
  
  age_spec <- age_spec[(still_bf_exclu) & (!hormonal_contracep_cur) ]
  
  #### 2- Design-based, glm with spline
  glm_age_spec <- NA
  if(!restrict_rural){
    try(
      glm_age_spec <- svyglm(still_ameno ~  splines::ns(age_interview, 4),
                             family=stats::quasibinomial(link = 'logit'),
                             design=data_svy,
                             subset= (still_bf_exclu & !hormonal_contracep_cur &
                                        age_interview <= PRED_TO_AGE_a)),
      silent=T
    )
  } else{
    try(
      glm_age_spec <- svyglm(still_ameno ~  splines::ns(age_interview, 4),
                             family=stats::quasibinomial(link = 'logit'),
                             design=data_svy,
                             subset= (still_bf_exclu & !hormonal_contracep_cur &
                                        residence=="rural" & 
                                        age_interview <= PRED_TO_AGE_a)),
      silent=T
    )
  }
  
  #### 3- Using GAMs
  
  
  if(restrict_rural){ dtb_loc <- dtb_loc[residence=="rural"] }
  
  dtb_loc[,norm_weight:= sample_weight/mean(sample_weight)]
  
  gam_age_spec <- mgcv::bam(still_ameno ~  s(age_interview, k=5, fx=T),
                            family = quasibinomial(link="logit"),
                            
                            weights=norm_weight,
                            
                            data=dtb_loc,
                            
                            subset=(still_bf_exclu & !hormonal_contracep_cur &
                                      age_interview <= PRED_TO_AGE_a ) )
  
  
  #### 4- Estimate time spent amenorrheic to age PRED_TO_AGE_a
  
  time_spent_f = function(w, data_a, PRED_TO_AGE_a_a, restrict_rural_a){
    
    data_ts = copy(data_a)
    
    data_ts[, W:=w]
    data_ts[, W_norm:=W/mean(W)]
    
    if(restrict_rural_a){data_ts <- data_ts[residence=="rural"]}
    
    gam_summary_loc <- mgcv::bam(still_ameno ~  s(age_interview, k=5, fx=T),
                                 family = quasibinomial(link="logit"),
                                 weights=W_norm,
                                 
                                 data=data_ts,
                                 
                                 subset=(still_bf_exclu & !hormonal_contracep_cur &
                                           age_interview <=PRED_TO_AGE_a ) )
    
    newdata = data.table(age_interview=seq(0, PRED_TO_AGE_a_a, 0.05))
    pred <- predict(gam_summary_loc, newdata, type = 'response')
    
    return( sum(pred)*0.05 )
  }
  
  time_spent_pred <- withReplicates(data_svyrep,
                                    time_spent_f,
                                    PRED_TO_AGE_a_a  = PRED_TO_AGE_a,
                                    restrict_rural_a = restrict_rural)
  
  #### predictions without SE
  newdata = data.table(age_interview=seq(0, PRED_TO_AGE_a, 0.05))
  time_spent_glm <- time_spent_gam <- NA_real_
  
  time_spent_gam <- predict(gam_age_spec, newdata, type = 'response')
  time_spent_gam <- sum(time_spent_gam)*0.05
  
  if(!is.na(glm_age_spec)){
    time_spent_glm <- predict(glm_age_spec, newdata, type = "response")
    time_spent_glm <- sum(time_spent_glm)*0.05
  }
  
  
  cat("end run....\n")
  return( list(design_based=age_spec,
               gam=gam_age_spec,
               glm=glm_age_spec,
               time_spent_gam=  time_spent_gam,
               time_spent_glm=  time_spent_glm,
               time_spent_se = time_spent_pred)  )
  
} # end trend_A_BEX_f


ALL_SURVEYS <- c("ASI-G1", "ASI-G2", "ASI-G3", "ASI-G4",
                 "SSA-G1", "SSA-G2", "SSA-G3", "SSA-G4", "SSA-G5",
                 "AS2-G1", "AS2-G2", "AS2-G3", "AS2-G4")
# add restriction to rural
ALL_SURVEYS <- c(paste0(ALL_SURVEYS, "-all"), paste0(ALL_SURVEYS, "-rur"))

CL=makeCluster(12, type = "FORK")
clusterEvalQ(CL, {library(survey)})
clusterExport(CL, c("dtb_BEX", "Groups_studies"))

res_BEX <- parLapply(cl=CL,
                     X = ALL_SURVEYS,
                     fun =  trend_A_BEX_f,
                     Groups_studies_a=Groups_studies,
                     data_a=dtb_BEX,
                     PRED_TO_AGE_a = 6 )
names(res_BEX) <- ALL_SURVEYS
stopCluster(CL)


tsa_mean <- 30.25*sapply(res_BEX, function(x){x$time_spent_se})

tsa_se <- sapply(res_BEX, function(x){30.25*sqrt(attr(x$time_spent_se, "var"))  })

round(tsa_mean)
round(tsa_mean - 1.96* tsa_se)
round(tsa_mean + 1.96* tsa_se)


2*pnorm(-(tsa_mean["ASI-G1-all"]-tsa_mean["ASI-G4-all"])/sqrt(tsa_se["ASI-G1-all"]^2+tsa_se["ASI-G4-all"]^2))
2*pnorm(-(tsa_mean["SSA-G1-all"]-tsa_mean["SSA-G5-all"])/sqrt(tsa_se["SSA-G1-all"]^2+tsa_se["SSA-G5-all"]^2))




initial_plot_f <- function(leg_a=NULL, 
                           col_leg_a=NULL, max_age_a=6,
                           cex_a=2, ...){
  plot(0, 0, pch=20, col="white",
       xlab="Time since childbirth (months)",
       ylab="Proportion amenorrheic", cex.lab=1.8,
       axes=F, ylim=c(0.45, 1), xlim=c(0.7, max_age_a+0.2), ...)
  axis(1, at=seq(1, max_age_a, 1), tck=-0.02, cex.axis=1.5)
  axis(2, las=2, tck=0.015, cex.axis=1.5)
  abline(h=seq(0.4, 0.9, 0.1), lty=2, col="gray")
  
  legend(1.5, 0.72, legend = leg_a, col = col_leg_a, xjust = 1,
         pch=20, cex=cex_a,  pt.cex=3, bty="n") 
}


add_group_plot_f = function(group_def_ext_a="ASI-G1-all", 
                            col_a="red",
                            Groups_studies_a=Groups_studies,
                            results_a = res_BEX, max_age_a=6){
  
  region_loc <- substr(group_def_ext_a, 1, 3)
  g_loc <- substr(group_def_ext_a, 5, 6)
  
  restrict_rural <- ( substr(group_def_ext_a, 8, 10)=="rur")
  
  group_number <- as.numeric(substr(g_loc, 2, 2))
  
  
  surv_dt_loc <- Groups_studies_a %>% filter(region==region_loc) 
  
  local_results <- results_a[[group_def_ext_a]]
  
  
  if(restrict_rural){ 
    age_spec <- local_results$design_based[residence=="rural"] 
  }else{
    age_spec <- local_results$design_based
  }
  
  position_adj <- (group_number-5/2)/sd(1:5)*0.05
  
  age_spec[, points(age_interview+position_adj, still_ameno, col = col_a, pch=20, cex = 1.3,
                    ylim=c(0, 1))]
  age_spec[, error.bar(age_interview+position_adj,
                       still_ameno,
                       se,
                       col = col_a)]
  
  newdata <- data.table(age_interview=seq(0, max_age_a+.1, 0.05))
  y_gam <- predict(local_results$gam, newdata, type = 'response')
  lines(newdata$age_interview, y_gam, col = col_a, lwd=2)
  
}


pal5 <- rev(wesanderson::wes_palette("Zissou1", 5, type = c("continuous")))


pdf("../ms/figs/Fig3.pdf", width = 12, height = 13)
par(mfrow=c(2,1), mar=c(5, 6, 2, 1))

mean_years_loc <- Groups_studies%>% filter(region=="ASI")%>%
  summarize( across(G1y:G4y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5[-3] )

add_group_plot_f("ASI-G1-all", pal5[1])
add_group_plot_f("ASI-G2-all", pal5[2])
add_group_plot_f("ASI-G3-all", pal5[4])
add_group_plot_f("ASI-G4-all", pal5[5])
mtext("Asia", line=-1, font=2, cex = 2)

mean_years_loc <- Groups_studies%>% filter(region=="SSA")%>%
  summarize( across(G1y:G5y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5 )

add_group_plot_f("SSA-G1-all", pal5[1])
add_group_plot_f("SSA-G2-all", pal5[2])
add_group_plot_f("SSA-G3-all", pal5[3])
add_group_plot_f("SSA-G4-all", pal5[4])
add_group_plot_f("SSA-G5-all", pal5[5])
mtext("Sub-Saharan Africa", line=-1, font=2, cex = 2)

dev.off()


pdf("../ms/figs_sup/FigS8.pdf", width = 12, height = 13)
par(mfrow=c(2,1), mar=c(5, 6, 2, 1))

mean_years_loc <- Groups_studies%>% filter(region=="ASI")%>%
  summarize( across(G1y:G4y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5[-3] )

add_group_plot_f("ASI-G1-rur", pal5[1])
add_group_plot_f("ASI-G2-rur", pal5[2])
add_group_plot_f("ASI-G3-rur", pal5[4])
add_group_plot_f("ASI-G4-rur", pal5[5])
mtext("Asia - rural residence", line=-1, font=2, cex = 2)


mean_years_loc <- Groups_studies%>% filter(region=="SSA")%>%
  summarize( across(G1y:G5y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5 )

add_group_plot_f("SSA-G1-rur", pal5[1])
add_group_plot_f("SSA-G2-rur", pal5[2])
add_group_plot_f("SSA-G3-rur", pal5[3])
add_group_plot_f("SSA-G4-rur", pal5[4])
add_group_plot_f("SSA-G5-rur", pal5[5])
mtext("Sub-Saharan Africa - rural residence", line=-1, font=2, cex = 2)

dev.off()



pdf("../ms/figs_sup/FigS9.pdf", width = 12, height = 13)
par(mfrow=c(2,1), mar=c(5, 6, 2, 1))

mean_years_loc <- Groups_studies%>% filter(region=="ASI")%>%
  summarize( across(G1y:G4y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5[-3], cex_a = 1.4 )

add_group_plot_f("AS2-G1-all", pal5[1])
add_group_plot_f("AS2-G2-all", pal5[2])
add_group_plot_f("AS2-G3-all", pal5[4])
add_group_plot_f("AS2-G4-all", pal5[5])
mtext("Asia (India removed)", line=-1, font=2, cex = 2)


mean_years_loc <- Groups_studies%>% filter(region=="ASI")%>%
  summarize( across(G1y:G4y, mean, na.rm=T))

initial_plot_f(leg_a = as.integer(mean_years_loc), col_leg_a = pal5[-3], cex_a = 1.4 )

add_group_plot_f("AS2-G1-rur", pal5[1])
add_group_plot_f("AS2-G2-rur", pal5[2])
add_group_plot_f("AS2-G3-rur", pal5[4])
add_group_plot_f("AS2-G4-rur", pal5[5])
mtext("Asia (India removed) - rural residence", line=-1, font=2, cex = 2)

dev.off()




