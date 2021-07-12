
##############################################################################
##############################################################################
####################### Bongaarts-Potter funtion (BPF) ####################### 

# function proposed by Bongaarts and Potter to model the   
# link between breastfeeding and amenorrhea
BPF_f <- function(x){ 1.753*exp(0.1396*x-0.001872*x*x) }

# inverse BPF
inv_BPF_f <- function(x){
  
  a <- 0.001872
  b <- -0.1396
  c <- log(x/1.753)
  
  delta <- b^2-4*a*c
  
  res <- (-b-sqrt(delta))/(2*a)
  
  if(res<0){
    res <- (-b+sqrt(delta))/(2*a)
  }
  
  if(abs(BPF_f(res)-x)>1e-4){cat("Incorrect result")}
  
  return(res)
}

