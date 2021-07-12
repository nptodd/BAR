######################################################################
##                 Retrieve main indicators from API                ##
######################################################################

indic_bf <- c( "FE_PPIS_W_MNA", "FE_PPID_W_MDA", # amenorrhea
               "CN_BFDR_C_MNA", "CN_BFDR_C_MNE", "CN_BFDR_C_MNP", # mean BF : Any, Excl, Pred
               "CN_BFDR_C_MDA", "CN_BFDR_C_MDE", "CN_BFDR_C_MDP",  # median BF : Any, Excl, Pred
               "CN_BRFS_C_EXB" # among (youngest) children under 2(3) liv. with mother
)


ESTIM_API <- dhs_data(indicatorIds = indic_bf)

table(ESTIM_API[,paste(IndicatorId, ":", Indicator)])

rm(indic_bf)

# dhs_indicators(indicatorIds = indic_bf, returnFields=c("IndicatorId","Label","Definition"))

ESTIM_API[datasets, on=.(SurveyId=SurveyId_original), `:=`(country = DHS_CountryCode,
                                                           survey_year_label = SurveyYearLabel,
                                                           dhs_phase = substr(FileName, 5,5) )]

ESTIM_API <- dcast(ESTIM_API, 
                   SurveyId + SurveyYear + CountryName ~ IndicatorId, 
                   value.var="Value")

setnames(ESTIM_API, 
         old = c("CN_BFDR_C_MNA", "CN_BFDR_C_MNE",
                 "CN_BFDR_C_MDA", "CN_BFDR_C_MDE",
                 "FE_PPIS_W_MNA", "FE_PPID_W_MDA"), 
         new = c("BANY_api", "BEX_api", 
                 "BANYmd_api", "BEXmd_api", # medians
                 "A_api", "Amd_api"))


# if survey broken down (India), then results evidently not given by API. Removed
ncores = 5
CL = makeCluster(ncores)
invisible(clusterEvalQ(CL, {library(data.table); library(survey)}))
clusterExport(CL, c("plot_ctry_f", "estimate_f", "BPF_f", "datasets") )
ESTIM_COMPARISON <- parLapply(cl = CL, 
                              X = dtb[SurveyId_original==SurveyId_mod,unique(DHS_CountryCode_mod)], 
                              fun = ctry_prof_f, 
                              type_a="guide",
                              to_file_a = F)
ESTIM_COMPARISON <- rbindlist(ESTIM_COMPARISON)
stopCluster(CL)

ESTIM_API[ESTIM_COMPARISON, on=.(SurveyId=SurveyId_mod), 
          `:=`(A_est = A, 
               BANY_est = BANY, 
               BEX_est = BEX,
               Amd_est = Amd, 
               BANYmd_est = BANYmd, 
               BEXmd_est = BEXmd,
               Region_col = Region_col)]



######################################################################
##                  Interactive plot with API data                  ##
######################################################################

# ESTIM_API  %>% plot_ly(x = ~BANY_api, name="")  %>%
#   layout(xaxis = list(title = "Mean duration of breastfeeding"),
#          yaxis = list(title = "Mean duration of postpartum amenorrhea")) %>%
#   add_markers(y = ~A_api, color=~Region_col,
#               text= ~paste(CountryName, SurveyYear) )


######################################################################
##           Comparison of self-computed and API ESTIM          ##
######################################################################


compare_est_f <- function(X, ...){
  plot(X[,c(1, 2)],  # first two columns
       col=X[, Region_col], 
       pch=20, cex=1.3, cex.lab=1.5, 
       axes=F, xlab="DHS program estimate (months)", 
       ylab="Estimate (months)", ...)
  abline(0, 1, lty=2, col="dimgrey")
  # axis(1, at=seq(0,24, 3), cex.axis=1.5)
  axis(1, cex.axis=1.5)
  if(max(X[,c(2)], na.rm = T)<8){  
    axis(2, las=2, cex.axis=1.5, tck=0.02) 
  } else{
    axis(2, las=2, cex.axis=1.5, tck=0.02, at = seq(0, max(X[,c(2)], na.rm=T), 6 )) 
  }
  
}


########## Check medians (computed the same way in both API and own estimates)

pdf("../ms/figs_sup/FigS10.pdf", width = 7, height = 7)
# par(mfrow=c(1,3))
# compare_est_f(ESTIM_API[,.(BANYmd_api, BANYmd_est, Region_col)])
# 
# title("A- Breastfeeding", cex.main=1.6, adj=0)

compare_est_f(ESTIM_API[,.(BEXmd_api, BEXmd_est, Region_col)], 
              xlim=c(0, ESTIM_API[,max(BEXmd_api, na.rm = T)]),
              ylim=c(0,ESTIM_API[,max(BEXmd_est, na.rm = T)]))

summary(ESTIM_API[, lm(BEXmd_api ~ BEXmd_est)])
cor(ESTIM_API[,.(BEXmd_api, BEXmd_est)], use = 'comp')
# title("B- Exclusive breastfeeding", cex.main=1.6, adj=0)
# title("Exclusive breastfeeding", cex.main=1.6)

# pb_surv = ESTIM_API[BEXmd_api > 9]
# text(pb_surv[,.(BEXmd_api, BEXmd_est)], 
#      labels = substr(pb_surv[, SurveyId], 1, 6), 
#      col = pb_surv[, Region_col],
#      cex=1.3, font=2,pos=1)
# rm(pb_surv)
# small but real differences
ESTIM_API[BEXmd_api < 9 & abs(BEXmd_api-BEXmd_est)>0.5,
          .(SurveyId, CountryName, SurveyYear, BEXmd_api, BEXmd_est=round(BEXmd_est, 1), diff=round(abs(BEXmd_api-BEXmd_est), 1) ) ]
# example. Compare API and own estimates' likelihood given other values
ESTIM_API[CountryName=="Egypt", .(SurveyYear, BEXmd_api, BEXmd_est=round(BEXmd_est,1) )]


# compare_est_f(ESTIM_API[,.(Amd_api, Amd_est, Region_col)])
# title("C- Postpartum amenorrhea", cex.main=1.6, adj=0)
dev.off()


########## Check means

# compare_est_f(ESTIM_API[,.(BANY_api, BANY_est, Region_col)], main="Mean duration of breastfeeding")
# compare_est_f(ESTIM_API[,.(A_api, A_est, Region_col)], main="Mean duration of amenorrhea")
# compare_est_f(ESTIM_API[,.(BEX_api, BEX_est, Region_col)], main="Mean duration of exclusive breastfeeding")
# abline(-0.75, 1)

# ESTIM_API[, mean(BANY_api-BANY_est, na.rm=T)]
# ESTIM_API[, mean(BEX_api-BEX_est, na.rm=T)]
