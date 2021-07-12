
# load packages, useful functions, etc.
source("shared.R")
source("../R/load/BPF_f.R")


# ------------------------------------------------------------- #
# ------- load computations have been run on GWDG cluster ------#


datasets <- fread("../data/final_data/datasets.csv")

COUNTRY <- fread("../results_cluster/COUNTRY.csv")

# add dev info 
DEV_TABLE <- fread("../data/final_data/DEV_TABLE.csv")
COUNTRY[DEV_TABLE, on=.(SurveyId_mod), 
        `:=`(hdi=hdi, 
             mean_bord=mean_bord)]

# results computed locally, no resampling
COUNTRY_NO_SE <- fread("../results/ESTIM_NO_HC.csv") 



# ------------------------------------------------------------- #
# ------------- check computations run on cluster ------------- #

COUNTRY_NO_SE[DHS_CountryCode_mod=="BD" & SurveyYear %in% c(1975, 2014, 2017)]
COUNTRY[DHS_CountryCode_mod=="BD" & SurveyYear %in% c(1975, 2014, 2017)]


# merge and check results
COUNTRY_NO_SE <- merge(COUNTRY_NO_SE, COUNTRY, by = c("SurveyId_mod"))
COUNTRY_NO_SE[, plot(BANY.x, BANY.y)]
abline(0, 1, lwd=2)
COUNTRY_NO_SE[, plot(BEX.x, BEX.y)]
abline(0, 1, lwd=2)
COUNTRY_NO_SE[, plot(A.x, A.y)]
abline(0, 1, lwd=2)

# get 95% UI for Bangladesh
COUNTRY[DHS_CountryCode_mod=="BD" & SurveyYear %in% c(1975, 2014, 2017), 
        .(SurveyYear, A, LOW=A-1.96*A_se, HIGH=A+1.96*A_se)]


# ------------------------------------------------------------- #
# ----------------- run meta-regression model ----------------- #

# Fit the models
data_SSA <- COUNTRY[Region_code=="SSA"]
data_ASI <- COUNTRY[Region_code=="ASI"]
data_LAC <- COUNTRY[Region_code%in%c("LAC", "EUR")]

model_SSA <- stan(file = "stan/country.stan",
                  data = data_stan_f(data_SSA),
                  refresh=500)
model_ASI <- stan(file = "stan/country.stan",
                  data = data_stan_f(data_ASI),
                  refresh=500)
model_LAC <- stan(file = "stan/country.stan",
                  data = data_stan_f(data_LAC),
                  refresh=1000)


relevant_coefs <-  c("c_intercept",  "c_bex", "c_mean_bord", "c_decade",
                     "c_hdi_mod", 
                     "c_bany", "c_bany_hdi_mod", 
                     "sigma_relationship") 

print(model_SSA,  relevant_coefs)
print(model_ASI,  relevant_coefs)
print(model_LAC,  relevant_coefs)



## ------ extract model simulations
simulations_SSA <- extract(model_SSA)
simulations_ASI <- extract(model_ASI)
simulations_LAC <- extract(model_LAC)


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

plot_comp_f(model_SSA, data_SSA, simulations_SSA, "Sub-saharan Africa")
plot_comp_f(model_ASI, data_ASI, simulations_ASI,  "Asia")
plot_comp_f(model_LAC, data_LAC, simulations_LAC,  "Latin America - Europe")


## ------ Check time trend

x_decade <- data_stan_f()$decades_for_pred
x_plot <- x_decade*10+1975

plot(-10,-10, xlim=range(x_plot), 
     ylim=c(-4, 3), xlab="Survey Year", 
     ylab="Effect on amenorrhea duration (months)", axes=F)
axis(1)
axis(2, las=2, tck=0.02)

col_SSA <- datasets[Region_code=="SSA"][1,Region_col]
col_ASI <- datasets[Region_code=="ASI"][1,Region_col]
col_LAC <- "lightpink2"

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
beepr::beep(1)


## ------ Get C_i index under low and high HDI settings

### estimate C_i at sensible hdi values

COUNTRY[Region_code == "SSA", range(hdi)]
COUNTRY[Region_code == "ASI", range(hdi)]
COUNTRY[Region_code  %in% c("LAC", "EUR"), range(hdi)]

COUNTRY[Region_code == "SSA", round(range(BANY, na.rm = T))]
COUNTRY[Region_code == "ASI", round(range(BANY, na.rm = T))]
COUNTRY[Region_code %in% c("LAC", "EUR"), round(range(BANY, na.rm = T))]

COUNTRY[Region_code %in% c("SSA", "ASI"), median(BEX, na.rm=T)]

### plot function
compute_C_i_f = function(simulations_a,
                         
                         range_bany_a=c(15, 34), 
                         HDI_range_a=c(0.3, .60),
                         UI_Ci_a=c(5/100, 95/100),
                         UI_Ciratio_a=c(2.5, 97.5)/100,
                         
                         cex_lab_a=1.8,
                         
                         tick_data_a=NULL, # plot hdi distribution
                         hdi_limits_a = c(0, 0.4, 0.5, 0.6, 1) # colors for hdi distribution
){
  
  CEX_AXIS <- 1.4
  
  BANY_new <- t(seq(min(range_bany_a), max(range_bany_a), 0.1))
  
  intercept_low <- with(simulations_a, c_intercept + 
                          
                          decade_effect[, data_stan_f()$decades_for_pred==1] + 
                          
                          c_bex*2.5 + c_mean_bord*2.5 + c_hdi_mod*(HDI_range_a[1]-0.2)*10 )
  
  slope_low <- with(simulations_a, c_bany + c_bany_hdi_mod*(HDI_range_a[1]-0.2)*10)
  
  intercept_high <- with(simulations_a, c_intercept +
                           
                           decade_effect[, data_stan_f()$decades_for_pred==1] + 
                           
                           c_bex*2.5 + c_mean_bord*2.5 + c_hdi_mod*(HDI_range_a[2]-0.2)*10)
  
  slope_high <- with(simulations_a, c_bany + c_bany_hdi_mod*(HDI_range_a[2]-0.2)*10)
  
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
  
  par( mfrow=c(1,2), mar=c(5, 5, 4, 1.5) + 0.1)
  
  plot(-10,-10, pch=20,
       xlim = range_bany_a, ylim=c(0, 25),
       xlab = "Duration of breastfeeding (months)", 
       ylab="Duration of amenorrhea (months)",  cex.lab=cex_lab_a,
       axes=F)
  axis(1, at=seq(ceiling(range_bany_a[1]/3)*3, range_bany_a[2], 3), cex.axis=CEX_AXIS)
  axis(2, las=2, tck=0.03, at=seq(0, 24, 3), cex.axis=CEX_AXIS)
  
  call_tcks = function(offset_y_a=c(1, 2)){
    pal <- sapply(c("red", "darkorange", "chartreuse3", "chartreuse4"), 
                  col.alpha, alpha= 0.4)
    hdi_tck <- tick_data_a[,hdi]
    y_tck = (findInterval(hdi_tck, hdi_limits_a)-offset_y_a[1])/offset_y_a[2]
    
    col_tck <- pal[ findInterval(hdi_tck, hdi_limits_a) ]
    
    # plot(hdi_tck, rep(1, length(hdi_tck)), col=col_tck , pch=20) # check
    points(x = tick_data_a[, BANY],
           y = y_tck, 
           col= col_tck,  pch=15, cex=0.8)
  }
  
  # if(!is.null(tick_data_a)){ call_tcks()}
  
  abline(a = mean(intercept_low), b = mean(slope_low), lwd=1.5, 
         col=col.alpha("red", 0.7))
  abline(a = mean(intercept_high), b = mean(slope_high), lwd=1.5,
         col=col.alpha("chartreuse4", 0.7))
  
  for(J in sample(length(intercept_low), 500, FALSE) ){
    abline(a = intercept_low[J], b = slope_low[J], 
           col=col.alpha("red", 0.03))
    abline(a = intercept_high[J], b = slope_high[J], 
           col=col.alpha("chartreuse4", 0.03))
  }
  curve(BPF_f, from = range_bany_a[1]-1, to= range_bany_a[2]+1,
        add=T, lwd=2, col="dimgrey")
  
  leg_def = c(paste0("Prediction for ", c("low", "high"),
                     " HDI (=", HDI_range_a, ")"), "Bongaarts-Potter function")
  
  legend(range_bany_a[1]+0.5, 25, leg_def, 
         col=c("red", "chartreuse4", "dimgrey"),
         cex=1.3, pt.cex=c(2.5,2.5,NA) ,
         pch=c(15, 15, NA), 
         lty=c(NA,NA, 1), lwd=c(NA,NA, 1.5), seg.len=0.6, bty="n")
  
  ##### RIGHT PLOT
  
  par(mar=c(5, 4.6, 4, 5.5) + 0.1)
  
  plot(BANY_new, Ci_low_mean, 
       col="red", 
       type="l", lwd=2, cex.lab=cex_lab_a,
       axes=F, range_bany_a,  xlab="Duration of breastfeeding (months)",
       ylim=c(0.5, 0.95), ylab=expression(C[i]))
  
  axis(1, at=seq(ceiling(range_bany_a[1]/3)*3, range_bany_a[2], 3), cex.axis=CEX_AXIS)
  axis(2, las=2, tck=0.03)
  lines(BANY_new, Ci_high_mean, pch=20, col="chartreuse4", lwd=2)
  
  
  # lines(BANY_new, Ci_low_lUI, col="red", lty=2)
  # lines(BANY_new, Ci_low_hUI, col="red", lty=2)
  rethinking::shade(t(matrix(c(Ci_low_lUI, Ci_low_hUI), ncol=2, byrow = F)), 
                    BANY_new, col=col.alpha("red",0.5) )
  # lines(BANY_new, Ci_high_lUI, col="chartreuse4", lty=2)
  # lines(BANY_new, Ci_high_hUI, col="chartreuse4", lty=2)
  rethinking::shade(t(matrix(c(Ci_high_lUI, Ci_high_hUI), ncol=2, byrow = F)), 
                    BANY_new, col=col.alpha("chartreuse4",0.5) )
  
  leg_def <-  c(expression(C[i]~" for low HDI") , expression(C[i]~" for high HDI"), 
                "Effect of low -> high HDI change on TFR")
  
  legend(range_bany_a[1]+0.5, 0.95,
         leg_def, 
         col=c("red", "chartreuse4", "dimgrey"),
         cex=1.3, pt.cex=c(2.5,2.5,NA) ,
         pch=c(15, 15, NA), 
         lty=c(NA,NA, 1), lwd=c(NA,NA, 1.5), seg.len=0.6,
         bty="n")
  
  # if(!is.null(tick_data_a)){ call_tcks(offset_y_a=c(1-100/2, 100))}
  
  
  par(new=TRUE)
  plot(BANY_new, 100*(Ci_ratio_mean-1), 
       type="l", lty=2, col="dimgrey", lwd=2, 
       axes=F, xlab="", ylab="",
       ylim=c(0, 30) )
  
  rethinking::shade(t(matrix(100*c(Ci_ratio_lUI-1, Ci_ratio_hUI-1), ncol=2, byrow = F)),
                    BANY_new, col=col.alpha("dimgrey",0.5) )
  
  axis(4, las=2, tck=0.02, ylim=c(0.95, 1.15), cex.axis=CEX_AXIS)
  mtext("Increase in TFR (%)", 4, line = 3, cex = cex_lab_a)
}



pdf("../ms/figs_sup/FigS5A.pdf", width = 16, height = 8)
compute_C_i_f(simulations_SSA)
mtext("A- Sub-Saharan Africa", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()

pdf("../ms/figs_sup/FigS5B.pdf", width = 16, height = 8)
compute_C_i_f(simulations_ASI)
mtext("B- Asia-Pacific", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()

# !!!! range
pdf("../ms/figs_sup/FigS5C.pdf", width = 16, height = 8)
compute_C_i_f(simulations_LAC, range_bany_a = c(9, 22))
mtext("C- Other regions", side = 3, line = -3, outer = TRUE, font=2, cex = 1.8)
dev.off()

