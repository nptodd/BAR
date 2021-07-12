
# load packages, useful functions, etc.
source("shared.R") 


# ------------------------------------------------------------- #
# --------- computations have been run on GDWG cluster -------- #

datasets <- fread("../data/final_data/datasets.csv")

ENERGY_PARITY <- fread("../results_cluster/ENERGY4_PARITY.csv")


DEV_TABLE <- fread("../data/final_data/DEV_TABLE.csv")
ENERGY_PARITY[DEV_TABLE, on=.(SurveyId_mod), 
              `:=`(hdi=hdi)]


# SCAM fitting errors 
ENERGY_PARITY[, .N, by=(is.na(BANY) | is.na(A))]
ENERGY_PARITY <- ENERGY_PARITY[!is.na( A *BANY)  ]
ENERGY_PARITY[, .N, by=(is.na(BEX))]
ENERGY_PARITY <- ENERGY_PARITY[!((BANY<2 & A > 5)|(BANY>30 & A <2)) ] # 1 group
 
# number of groups analyzed
ENERGY_PARITY[,.N]
ENERGY_PARITY[,.N, by=Region_code]
ENERGY_PARITY[,length(unique(SurveyId_mod))] 


pdf("../ms/figs_sup/FigS6.pdf")
Region_col_mod <- sapply(ENERGY_PARITY[, Region_col], col.alpha, 0.4)
ENERGY_PARITY[,plot(BANY, A, col=Region_col_mod,
                    pch=20, cex=0.6, xlim=c(0, 40), ylim=c(0, 25), axes=F,
                    xlab="Duration of breastfeeding (months)",
                    ylab="Duration of postpartum amenorrhea (months)")]
axis(1, at=seq(0, 36, 6))
axis(2, at=seq(0, 24, 6), las= 2, tck=0.02)
dev.off()


### Data for models

data_SSA <- ENERGY_PARITY[Region_code=="SSA"]
data_ASI <- ENERGY_PARITY[Region_code=="ASI"]
data_LAC <- ENERGY_PARITY[Region_code %in% c("LAC", "EUR")]


### Fit models

hdi_energy_SSA <- stan(file = "stan/hdi_energy4_parity.stan",
                       data = data_stan_f(data_SSA) ,
                       iter=4e3 )

hdi_energy_ASI <- stan(file = "stan/hdi_energy4_parity.stan", 
                       data = data_stan_f(data_ASI) ,
                       iter=4e3 )

hdi_energy_LAC <- stan(file = "stan/hdi_energy4_parity.stan", 
                       data = data_stan_f(data_LAC) ,
                       control = list(adapt_delta = 0.99, max_treedepth = 15),
                       iter=4e3 )

relevant_coefs_hdi <-  c("c_bany",
                         
                         "c_bany_hdi_mod","c_bany_urban", "c_bany_elec", "c_bany_water", "c_bany_educ",
                         "c_bany_par1", "c_bany_par4",
                         
                         "c_hdi_mod", "c_urban", "c_elec", "c_water", "c_educ",
                         
                         "c_par1",  "c_par4","c_bex", 
                         
                         "c_decade", "c_intercept", "sigma_relationship",
                         
                         "combined_full1hdi_slope", "combined_full1hdi_intercept")

print(hdi_energy_SSA,  relevant_coefs_hdi, probs=c(2.5, 97.5)/100)
print(hdi_energy_ASI,  relevant_coefs_hdi, probs=c(2.5, 97.5)/100)
print(hdi_energy_LAC,  relevant_coefs_hdi, probs=c(2.5, 97.5)/100)


## ------ extract model simulations
simulations_SSA <- extract(hdi_energy_SSA)
simulations_ASI <- extract(hdi_energy_ASI)
simulations_LAC <- extract(hdi_energy_LAC)


## ------ Check predictor - A concordance

plot_comp_f = function(model_a, data_a, simulations_a, title_a=""){
  
  sum_loc <- summary(model_a)$summary
  
  loc_check <-  data_stan_f(data_a, for_fit_a = F)
  loc_check[, predictor_normal:=NA_real_]
  
  for(I in seq_len(loc_check[,.N]) ){
    loc_check[I, predictor_normal:=sum_loc[paste0("predictor_normal[", I, "]"), "mean" ]]
  }
  
  loc_check[, plot(predictor_normal, A, pch=20, cex=0.8, 
                   xlab="Predicted duration of amenorrhea (months)", 
                   ylab="Measured duration of\n postpartum amenorrhea (months)",
                   cex.lab=1.5)]
  abline(0, 1); mtext(title_a, line=1, font=2)
  cat("variance explained: ", loc_check[, 1-var(A-predictor_normal)/var(A)], "\n")
}

par(mfrow=c(1, 3), mar=c(5, 6, 4, 1))
plot_comp_f(hdi_energy_SSA, data_SSA, simulations_SSA, "Sub-Saharan Africa")
plot_comp_f(hdi_energy_ASI, data_ASI, simulations_ASI,  "Asia")
plot_comp_f(hdi_energy_LAC, data_LAC, simulations_LAC,  "Latin America - Europe")


## ------ Investigate parity effect on example
mean(simulations_ASI$c_par4+20*simulations_ASI$c_bany_par4)
quantile(simulations_ASI$c_par4+20*simulations_ASI$c_bany_par4, 0.025)
quantile(simulations_ASI$c_par4+20*simulations_ASI$c_bany_par4, 0.975)

mean(simulations_SSA$c_par4+20*simulations_SSA$c_bany_par4)
quantile(simulations_SSA$c_par4+20*simulations_SSA$c_bany_par4, 0.025)
quantile(simulations_SSA$c_par4+20*simulations_SSA$c_bany_par4, 0.975)

mean(simulations_LAC$c_par4+20*simulations_LAC$c_bany_par4)
quantile(simulations_LAC$c_par4+20*simulations_LAC$c_bany_par4, 0.025)
quantile(simulations_LAC$c_par4+20*simulations_LAC$c_bany_par4, 0.975)


## ------ Check time trend

pdf("../ms/figs_sup/FigS7.pdf", width = 9, height = 6)

x_decade <- data_stan_f()$decades_for_pred

plot(-10,-10, xlim=range(x_decade*10+1975), 
     ylim=c(-4, 3), xlab="Survey Year", 
     ylab="Effect on amenorrhea duration (months)", axes=F)
axis(1)
axis(2, las=2, tck=0.02)


col_SSA <- datasets[Region_code=="SSA"][1,Region_col]
col_ASI <- datasets[Region_code=="ASI"][1,Region_col]
col_LAC <- "lightpink2"

x_plot <- x_decade*10+1975

draw_trend_f=function(simulations_loc, col_a){
  lines(x_plot, apply(simulations_loc$decade_effect, 2, mean), col=col_a, lwd=2)
  lines(x_plot, apply(simulations_loc$decade_effect, 2, quantile, probs=0.05), col=col_a, lty=2)
  lines(x_plot, apply(simulations_loc$decade_effect, 2, quantile, probs=0.95), col=col_a, lty=2)
  
  polygon(c(rev(x_plot), x_plot), 
          c(rev(apply(simulations_loc$decade_effect, 2, quantile, probs=0.25)), 
            apply(simulations_loc$decade_effect, 2, quantile, probs=0.75)), 
          col = col.alpha(col_a, 0.2), border = NA)
}

draw_trend_f(simulations_SSA, col_SSA)
draw_trend_f(simulations_ASI, col_ASI)
draw_trend_f(simulations_LAC, col_LAC)

legend("bottomleft", c("Sub-Saharan Africa", "Asia-Pacific", "Other regions"), pch="-", lwd=3, 
       col=c(col_SSA, col_ASI, col_LAC), cex=1.2,
       bty="n", ncol=3, text.width=c(5, 13, 11))
dev.off()


## ------ Get C_i index under low and high dev settings

compute_C_i_f = function(simulations_a,
                         
                         range_bany_a=c(11.5, 33.5),
                         UI_Ci_a=c(2.5, 97.5)/100,
                         UI_Ciratio_a=c(2.5, 97.5)/100,
                         
                         center_leg_a=F, # should right legend be centered 
                         
                         cex_lab_a=1.8,
                         mfrow_a=c(1,2)){
  
  CEX_AXIS <- 1.4
  
  BANY_new <- t(seq(min(range_bany_a), max(range_bany_a), 0.1))
  
  intercept_low <- with(simulations_a, c_intercept + 
                          
                          decade_effect[, data_stan_f()$decades_for_pred==1] + 
                          
                          c_bex*2.5 + c_hdi_mod*1 # at HDI = 0.3 -> HDI*=10*(0.3-0.2)=1
  )
  
  slope_low <- with(simulations_a, c_bany + c_bany_hdi_mod )
  
  intercept_high <- with(simulations_a, c_intercept + 
                           
                           decade_effect[, data_stan_f()$decades_for_pred==1] + 
                           
                           c_bex*2.5 + 
                           
                           combined_full3hdi_intercept)
  
  slope_high <- with(simulations_a, c_bany + combined_full3hdi_slope)
  
  A_low = matrix(data = intercept_low, 
                 nrow = length(intercept_low), 
                 ncol = ncol(BANY_new), byrow=F) + kronecker(slope_low,  BANY_new)
  A_high = matrix(data = intercept_high, 
                  nrow = length(intercept_high), 
                  ncol = ncol(BANY_new), byrow=F) + kronecker(slope_high, BANY_new)

  Ci_low <- apply(A_low, 1:2, Ci_f) 
  Ci_high <- apply(A_high, 1:2, Ci_f)
  Ci_low_mean <- apply(Ci_low, 2, mean)
  Ci_high_mean <- apply(Ci_high, 2, mean)
  
  Ci_low_lUI <- apply(Ci_low, 2, quantile, probs  = UI_Ci_a[1])
  Ci_low_hUI <- apply(Ci_low, 2, quantile, probs = UI_Ci_a[2])
  Ci_high_lUI <- apply(Ci_high, 2, quantile, probs = UI_Ci_a[1])
  Ci_high_hUI <- apply(Ci_high, 2, quantile, probs = UI_Ci_a[2])
  
  Ci_ratio <- Ci_high/Ci_low # for each posterior sample
  
  Ci_ratio_mean <- apply(Ci_ratio, 2, mean)
  Ci_ratio_lUI    <-  apply(Ci_ratio, 2, quantile, probs = UI_Ciratio_a[1])
  Ci_ratio_hUI   <- apply(Ci_ratio, 2, quantile, probs = UI_Ciratio_a[2])
  
  cat( "at BANY=30",
       "\n    Ci for low HDI:",  round(Ci_low_mean[as.vector(BANY_new)==30], 2), 
       "\n    2.5% bound:" ,     round(Ci_low_lUI[as.vector(BANY_new)==30], 2), 
       "\n    97.5% bound:" ,    round(Ci_low_hUI[as.vector(BANY_new)==30], 2), 
       
       "\n\n    Ci for high HDI:", round(Ci_high_mean[as.vector(BANY_new)==30], 2), 
       "\n    2.5% bound:" ,       round(Ci_high_lUI[as.vector(BANY_new)==30], 2), 
       "\n    97.5% bound:" ,      round(Ci_high_hUI[as.vector(BANY_new)==30], 2), 
       
       "\n\n    mean ratio:",  round(Ci_ratio_mean[as.vector(BANY_new)==30], 2), 
       "\n    2.5% bound:" , round(Ci_ratio_lUI[as.vector(BANY_new)==30], 2), 
       "\n    97.5% bound:" , round(Ci_ratio_hUI[as.vector(BANY_new)==30], 2), "\n")
  
  ##### LEFT PLOT
  if(!is.null(mfrow_a)){
    par( mfrow=mfrow_a, mar=c(5, 5, 4, 1.5) + 0.1)
  }else{
    par(mar=c(5, 5, 4, 1.5) + 0.1)
  }
  
  plot(-10,-10, pch=20,
       xlim = range_bany_a, ylim=c(0, 25),
       xlab = "Duration of breastfeeding (months)", 
       ylab="Duration of amenorrhea (months)",
       cex.lab=cex_lab_a, 
       axes=F)
  axis(1, at=seq(ceiling(range_bany_a[1]/3)*3, range_bany_a[2], 3), cex.axis=CEX_AXIS)
  axis(2, las=2, tck=0.03, at=seq(0, 24, 3), cex.axis=CEX_AXIS)
  
  abline(a = mean(intercept_low), b = mean(slope_low), lwd=1.5,
         col=col.alpha("red", 0.7))
  abline(a = mean(intercept_high), b = mean(slope_high),  lwd=1.5,
         col=col.alpha("chartreuse4", 0.7))
  
  for(J in sample(length(intercept_low), 500, FALSE) ){
    abline(a = intercept_low[J], b = slope_low[J], 
           col=col.alpha("red", 0.03))
    abline(a = intercept_high[J], b = slope_high[J], 
           col=col.alpha("chartreuse4", 0.03))
  }
  curve(BPF_f, from = range_bany_a[1]-1, to= range_bany_a[2]+1,
        add=T, lwd=2, col="dimgrey")
  
  leg_def = c("'Low development' situation", 
              "'Higher development' situation",
              "Bongaarts-Potter function")
  
  legend(range_bany_a[1]+0.5, 27, leg_def, y.intersp = 2,
         col=c("red", "chartreuse4", "dimgrey"),
         cex=1.3, pt.cex=c(2.5,2.5,NA) ,
         pch=c(15, 15, NA), 
         lty=c(NA,NA, 1), lwd=c(NA,NA, 2), seg.len=0.6,
         bty="n")

  ##### RIGHT PLOT
  par(mar=c(5, 4.6, 4, 5.5) + 0.1)
  
  plot(BANY_new[c(-1,-length(BANY_new)) ], 
       Ci_low_mean[c(-1,-length(BANY_new)) ], 
       col="red", 
       type="l", lwd=2, cex.lab=cex_lab_a,
       axes=F, range_bany_a,  xlab="Duration of breastfeeding",
       ylim=c(0.5, 0.95), ylab=expression(C[i]))
  
  axis(1, at=seq(ceiling(range_bany_a[1]/3)*3, range_bany_a[2], 3), cex.axis=CEX_AXIS)
  axis(2, las=2, tck=0.03, cex.axis=CEX_AXIS)
  lines(BANY_new[c(-1,-length(BANY_new)) ], 
        Ci_high_mean[c(-1,-length(BANY_new)) ],
        pch=20, col="chartreuse4", lwd=2)
  
  rethinking::shade(t(matrix(c(Ci_low_lUI, Ci_low_hUI), ncol=2, byrow = F)), 
                    BANY_new, col=col.alpha("red",0.5) )
  rethinking::shade(t(matrix(c(Ci_high_lUI, Ci_high_hUI), ncol=2, byrow = F)), 
                    BANY_new, col=col.alpha("chartreuse4",0.5) )
  
  leg_def <-  c(expression(C[i]~", 'Low development' situation") ,
                expression(C[i]~", 'Higher development' situation"), 
                "Development effect on TFR")
  
  x_leg_right <- ifelse(center_leg_a, 
                        range_bany_a %*% c(0.70, 0.30),
                        range_bany_a[1]+0.5)
  
  legend(x_leg_right+0.5, 0.98,
         leg_def,  y.intersp = 2,
         col=c("red", "chartreuse4", "dimgrey"),
         cex=1.3, pt.cex=c(2.5,2.5,NA) ,
         pch=c(15, 15, NA), 
         lty=c(NA,NA, 1), lwd=c(NA,NA, 2), seg.len=0.6,
         bty="n")
  
  par(new=TRUE)
  plot(BANY_new, 100*(Ci_ratio_mean-1), 
       type="l", lty=2, col="dimgrey", lwd=2, 
       axes=F, xlab="", ylab="",
       ylim=c(0, 30) 
  )
  rethinking::shade(t(matrix(100*c(Ci_ratio_lUI-1, Ci_ratio_hUI-1), ncol=2, byrow = F)),
                    BANY_new, col=col.alpha("dimgrey",0.5) )
  axis(4, las=2, tck=0.02, ylim=c(0.95, 1.15), cex.axis=CEX_AXIS)
  mtext("Increase in TFR (%)", 4, line = 3, cex = cex_lab_a)
  
}


pdf("../ms/figs/Fig2A.pdf", width = 16, height = 8)
compute_C_i_f(simulations_SSA, mfrow_a = )
mtext("Sub-Saharan Africa", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()

pdf("../ms/figs/Fig2B.pdf", width = 16, height = 8)
compute_C_i_f(simulations_ASI)
mtext("Asia-Pacific", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()

pdf("../ms/figs/Fig2C.pdf", width = 16, height = 8)
compute_C_i_f(simulations_LAC, range_bany_a = c(9, 22))
mtext("Other regions", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()


