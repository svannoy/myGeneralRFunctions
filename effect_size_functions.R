#********************************************************************************************
# compute standardized mean difference score along with confidence interval and pooled standard deviation
# NOTE: Only works for independent samples
#
cohd <- function(x = NULL,y=NULL, sd1=NULL, sd2=NULL, n1=NULL, n2 = NULL,  m1=NULL, m2=NULL, conf.level=.95){
  
  require(MBESS)

  # Check incoming arguments
  if ((is.null(x) | is.null(y)) & (is.null(m1) | is.null(m2))) {
    warning("Function cohd requires either two vectors of data (x, y), or two pre-calculated means (m1, m2)")
    return(NULL)
  }
  
  if ((!is.null(x) | !is.null(y)) & (!is.null(m1) | !is.null(m2))) {
    warning("Function cohd requires either two vectors of data (x, y), or two pre-calculated means (m1, m2), you have passed in both")
    return(NULL)
  }
  
  if (!is.null(x) & !is.null(y)){
    if(length(x) < 2 | length(y) < 2){
    warning("You have called cohd specifying two vectors (x, y) but at least one length is < 2")
    return(NULL)
    }
  }
  
  if (!is.null(m1) & !is.null(m2)){
    if(is.null(sd1) | is.null(sd2)){
    warning("You have called cohd specifying two means (m1, m2), so you must provide standard deviations (sd1, sd2)")
    return(NULL)
    }
  }
  
  # We know now, that if we don't have an x, we are relying on pre-computed means so we need n's to be specified 
  if (is.null(x)){
    if (is.null(n1) | is.null(n2)){
    warning("If you call cohd with pre-computed means, you must also pass in the size of each group")
    return(NULL)
    }
  }
  
    # If they pass vectors, then calculate the sd
  if (is.null(n1)) {
    n1 <- length(x)
    n2 <- length(y)
  }

  # Compute difference between the means
  if (is.null(m1)){
    meanChange = mean(x)-mean(y)
  }else {
    meanChange = m1 - m2
  }

  # Compute pooled Standard Deviation in 3 parts just for readability
  if (is.null(m1)){
    pooledSd = mypooledSd(x=x, y=y)
  }
  else {
    pooledSd = mypooledSd(m1=m1, m2=m2,sd1=sd1,sd2=sd2,n1=n1,n2=n2)
  }
  
  # Now we can compute cohen's d
  d = meanChange/pooledSd
 
  # For the confidence interval: Credit to "MBESS", this info is easily accessible on the net, but I found it here as well
  dCI <- ci.smd(smd=d, n.1=n1, n.2=n2, conf.level=conf.level)
  lowerD <-dCI$Lower.Conf.Limit.smd
  upperD = dCI$Upper.Conf.Limit.smd
  
  returnVal <- list(d, pooledSd, meanChange, lowerD, upperD)
  names(returnVal) <- c("smd", "pooledSd", "M2-M1", "lowerSMD", "upperSMD")
  return(returnVal)
}

### Function to compute pooled standard deviation ####
## You can call this with two vectors of data (x and y),
## or you can call with pre-computed means and standard deviations.
## In any case you must pass in the size of each sample (n1 and n2)

mypooledSd <- function(x = NULL, y = NULL, m1=NULL, m2=NULL, sd1=NULL, sd2=NULL, n1=NULL, n2=NULL, conf.level=0.95){

  pooledSd <- NULL
  
  # Check incoming arguments
  if ((is.null(x) | is.null(y)) & (is.null(m1) | is.null(m2))) {
    warning("Function mypooledSd requires either two vectors of data (x, y), or two pre-calculated means (m1, m2)")
    return(NULL)
  }
  
  if ((!is.null(x) | !is.null(y)) & (!is.null(m1) | !is.null(m2))) {
    warning("Function mypooledSd requires either two vectors of data (x, y), or two pre-calculated means (m1, m2), you have passed in both")
    return(NULL)
  }
  
  if (!is.null(x) & !is.null(y)){
    if(length(x) < 2 | length(y) < 2){
    warning("You have called mypooledSd specifying two vectors (x, y) but at least one length is < 2")
    return(NULL)
    }
  }
  
  if (!is.null(m1) & !is.null(m2)){
    if(is.null(sd1) | is.null(sd2)){
    warning("You have called mypooledSd specifying two means (m1, m2), so you must provide standard deviations (sd1, sd2)")
    return(NULL)
    }
  }
  
  # We know now, that if we don't have an x, we are relying on pre-computed means so we need n's to be specified 
  if (is.null(x)){
    if (is.null(n1) | is.null(n2)){
    warning("If you call mypooledSd with pre-computed means, you must also pass in the size of each group")
    return(NULL)
    }
  }
  
  # Begin calculation ###
  
  # If they pass vectors, then calculate the sd
  if (is.null(x) == FALSE) {
      sd1 <- sd(x)
      sd2 <- sd(y)
      n1 <- length(x)
      n2 <- length(y)
  }
  
  # Compute pooled Standard Deviation in 3 parts just for readability
  dv1 = (n1-1)*(sd1^2)
  dv2 = (n2-1)*(sd2^2)
  pooledSd = sqrt((dv1+dv2)/(n1 + n2 -2))

  return(pooledSd)
}

