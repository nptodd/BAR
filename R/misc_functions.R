
# get last characters
substrRight <- function(x, n){ substr(x, nchar(x)-n+1, nchar(x)) }

# plot error bars
error.bar <- function(x, y, upper, lower=upper, length=0.0,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}


# get estimate from output of withReplicates
get_est <- function(x, replicates=F){
  if(replicates){
    return(  as.numeric(x$theta)   )
  } else {
    return(  as.numeric(x)  )
  }
}

# get estimate of standard error from object returned by withReplicates
get_se <- function(x, replicates=F){
  if(is.na(x)){
    return(  NA_integer_  )
  }
  if(replicates){
    return(  sqrt(as.numeric(attr(x$theta, "var")))  )
  } else {
    return(  sqrt(as.numeric(attr(x, "var")))  )
  }
}