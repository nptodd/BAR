library(rstan)
library(data.table)
library(survey)
library(scam)

options(mc.cores = parallel::detectCores())


rstan_options(auto_write = TRUE)
# https://discourse.mc-stan.org/t/r-goes-really-
# slow-implemented-rstan-dont-know-what-happened/19952/5
rstan_options (javascript = FALSE)


data_stan_f = function(dtb_a=NULL, for_fit_a=T){
  
  if(!is.null(dtb_a)){
  data_stan <- dtb_a[!(is.na(BANY*BANY_se* A*A_se* BEX*BEX_se))]
  
  } else {
    data_stan <- list()} # to get spline info anyway

  
  if(for_fit_a){
    data_stan <- as.list(data_stan)
    data_stan$N <- length(data_stan$BANY)
    data_stan$N_missed <- dtb_a[,.N] - data_stan$N
    
    # spline info
    data_stan$spline_degree <- 3
    data_stan$knots <- (seq(1975, 2021, 9)-1975)/10 
    data_stan$num_knots <- length(data_stan$knots)
    
    # decade effect global, for prediction
    data_stan$decades_for_pred <- (seq(1975, 2020, 0.5)-1975)/10
    data_stan$n_decades_for_pred <- length(data_stan$decades_for_pred)
    
  }
  
  return(data_stan)
}


col.alpha = rethinking::col.alpha


# Bongaarts Potter function
source("../R/load/BPF_f.R")
# contraceptive index
Ci_f = function(A_a){ 20/(18.5+A_a) }


# Functions for estimation
source("../R/bf/estimate_f.R")
source("summarize_surv_f.R")
source("../R/misc_functions.R")