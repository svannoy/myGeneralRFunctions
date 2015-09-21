###
# Function: getHolmsPvalues
# Purpose: Create an vector of successive p-values used for a "holm's" style multiple comparison correction for inflated type-i error
# Returns: a vector of p values from smallest to largest
# Arguments:
#   numComparisons - how many comparisons are being done
#   alpha - the desired probability of type I error (i.e. the critical p value), default is 0.05
getHolmsPvalues <- function(numComparisons, alpha = 0.05){
  
  # Verify we got the numComparisons argument)
  if (missing(numComparisons)){
     stop("You must specify the number of comparisons for getHolmsPvalues")
  }
  
  # Pre-allocate return value
  pValues <- numeric(numComparisons)
  
  # Loop through computing each successive p-value, not indexing reverses order so they come out as you would use them
  for (i in numComparisons:1){
    pValues[(numComparisons - i)+1] <- 1-(1-alpha)^(1/i)
  }
  
  return(pValues)
}


####
# Function: countSignificantTests_Holms
# Purpose: Determine the number of signficant comparisons using holm's method
# Arguments:
#   pValues: A vector of pValues computed
#   referenceP: The critical value for P, defaults to 0.05
#   addComps: A numeric value that 'extends' the number of comparisons to do.
#      addComps should generally be left unassigned. Consider, if you have 12 p-values
#      but they come from a family of 20 comparisons, you would set addComps to 8 so the
#      holms procedure is conducted assuming there were 20 comparisons
# Returns: A list
#   $numSignificant = Number of significant tests
#   $sigPIndexes = A vector of indexes for the significant p-values from the argument 'pValues'
#   $sigPValues = A vector of the p-values deemed significant
#   $holmsValues = A vector of p-values used by the Holms method for determining significance
#
countSignificantTests_Holms <- function(pValues, referenceP = 0.05, addComps = 0){
  
  sigP <- NA
  sigPValues <- NA
  
  if (missing(pValues)){
      stop("You called countSignificantTests_Holms without specifying pValues")
  }
  
  # Sort the pValues in decending order
  sortedPValues <- sort(pValues)
  holmsValues <- getHolmsPvalues(length(sortedPValues)+addComps, referenceP)
  
  i <- 1
  while((i <= length(sortedPValues)) & (sortedPValues[i] < holmsValues[i])){
    i <- i + 1
  }
  
  sigP <- which(pValues %in% sortedPValues[i-1:i])
  return(list(numSignificant = i-1, sigPIndexes = sigP, sigPValues = sortedPValues[1:i-1], holmsValues = holmsValues))
}

####
# Function: countSignificantTests_Bonferroni
# Purpose: Determine the number of signficant comparisons using standard Bonferonni method
# Arguments:
#   pValues: A vector of pValues computed
#   alpha - the desired probability of type I error (i.e. the critical p value), default is 0.05
#   addComps: A numeric value that 'extends' the number of comparisons to do.
#      addComps should generally be left unassigned. Consider, if you have 12 p-values
#      but they come from a family of 20 comparisons, you would set addComps to 8 so the
#      Bonferonni procedure is conducted assuming there were 20 comparisons
# Returns: A list
#   $numSignificant = Number of significant tests
#   $sigPIndexes = A vector of indexes for the significant p-values from the argument 'pValues'
#   $sigPValues = A vector of the p-values deemed significant
#   $holmsValues = A vector of p-values used by the Holms method for determining significance
#
countSignificantTests_Bonferroni <- function(pValues, alpha = 0.05, addComps = 0){
  
  sigP <- NA
  sigPValues <- NA
  
  if (missing(pValues)){
      stop("You called countSignificantTests_Holms without specifying pValues")
  }
  
  # Sort the pValues in decending order
  bonFerroniP  <- 1-(1-alpha)^(1/length(pValues)+addComps)

  
  numSigResults <- sum(pValues < bonFerroniP)
  indexSigResults <- which(pValues < bonFerroniP)
  
  return(list(numSignificant = numSigResults, sigPIndexes = indexSigResults, sigPValues = pValues[indexSigResults], bonFerroniP = bonFerroniP))
}

###
# Function: getTypeIErrProb
# Purpose: Return the probability of type-I error given number of simultaneous comparisons
# Arguments:
#   numComparisons: How many comparisons are being conducted
# Returns: Probability of type-I error as atomic numeric variable 
getTypeIErrProb <- function(numComparisons){
  
  # Verify we got the numComparisons argument)
  if (missing(numComparisons)){
     stop("You must specify the number of comparisons for getTypeIErrProb")
  }
  
  return(1-0.95^numComparisons)
}

###
# Function: plotSigP_Holms
# Purpose: plot p-values and their 'significance' status via the Holms Method
# Background: 
#   From Serlin and Lapsley 1993, we want to compute the confidence interval for the ncp if a null hypothesis is rejected
#   There is a function that can do this in SAS, named TNCONT, the function here was gleaned from info on the internet to replicate that function
plotSigP_Holms <- function(varNames=NULL, pValues=NULL){
require(ggplot2, quietly = TRUE)
  
  if(is.null(varNames) | is.character(varNames) == FALSE){
    stop("You must pass a character vector of varNames as first variable to function plotSigP_Holms")
  }
  if(is.null(pValues) | is.numeric(pValues) == FALSE){
    stop("You must pass a numeric vector of pValues as the second variable to function plotSigP_Holms")
  }
  
  holmsValues = getHolmsPvalues(length(pValues))
  results <- countSignificantTests_Holms(pValues)
  isSig <- seq(1:length(pValues)) %in% results$sigPIndexes
  dataForGraph <- data.frame(index = seq(1:length(pValues)), pValues = format(pValues, scientific=FALSE), names=varNames, holmsValues=holmsValues, isSig=isSig) # Don't know why I have to format those pValues to get them not to print on the screen in scientific notation, but for some reason today I am
  thePlot <- ggplot(data=dataForGraph, aes(x=index, y=pValues, fill=isSig))+
    geom_bar(stat="identity") +
    labs(x="Predictor", y="P-values", title=sprintf("You have %d significant p-value%s", sum(dataForGraph$isSig, na.rm=TRUE), ifelse(sum(dataForGraph$isSig, na.rm=TRUE) == 1, "", "s"))) +
    scale_x_reverse(breaks=dataForGraph$index, labels=dataForGraph$names) +
    scale_fill_discrete(name="Significant") +
    coord_flip()
  
  if(interactive()){
    print(thePlot)
  }
  else {
    thePlot
  }
}

###
# Function: tncont
# Purpose: find the non-centrality parpameter for a non-central t-distribution that corresponds to the stated probability, critical value, and degrees of freedom
# Background: 
#   From Serlin and Lapsley 1993, we want to compute the confidence interval for the ncp if a null hypothesis is rejected
#   There is a function that can do this in SAS, named TNCONT, the function here was gleaned from info on the internet to replicate that function
tncont <- function(deltaRange, pr, x, df) {
  
  # Function to calculate a given probability to our target
  comparePvalues <- function(delta, pr, x, df) pt(x, df=df, ncp=delta) - pr

  # Uniroot finds a minimum over a range applied to a target function
  # We subtrackt a tiny amount from 'pr' otherwise, it may select the value that goes just over 'pr', e.g. the root might be 0.9500001, but we'd want 0.949999
  root <- uniroot(f=comparePvalues, interval=deltaRange, check.conv = TRUE, pr-.000001, x, df)
  
  return(root$root)
}


###
# Function: getLamdaCI
# Purpose: Calculate the non-centrality parpameter confidence interval for a given lambda that results in rejection of the nully hypothesis
# Background: 
#   See  Serlin and Lapsley 1993
getLamdaCI <- function(lamda, tObserved, prob, df){

  # Note, the range on lamda is a guess, it would probably be best to put it in a loop and extend the range if null is returned
  # but, the uniroot doesn't return a flag variable if it doesn't converge so I set it up to create an error if it doesn't converge
  lamdaCrit <- tncont(c(lamda/20, lamda*20), pr = prob, x = tObserved, df=df)

  # Confirmtation that this is what we expect, just turn on if we need to debug
 # pt(tObserved, N-2, lamdaCrit) # Is close approximation to 0.95

  # Finally the effect size confidence interval is >= maxEffSizeofNoInterest
  maxEffSizeOfNoInterest <- lamdaCrit/sampleSizeAdjustment
  
  return(maxEffSizeOfNoInterest)
}

###
# Function: testCountSignificantTests_Holms
# Purpose: Tests the functionality of the countSignificantTests_Holms function
# Arguments: None
# Returns: Nothing
# Comments: Use this function to test cht countSignficantTests_Holms function, write code, fool around, etc. just keep it contained here
testCountSignificantTests_Holms <- function(){

# Example of using the above functions from the data in Arpin et al from our review
pValues <- c(0.001, 0.1200, 0.0010, 0.0010,
             0.0010, 0.0010, 0.0690, 0.0280,
             0.5650, 0.0130, 0.0310, 0.0460,
             0.0020, 0.0390, 0.0320, 0.1040,
             0.1000, 0.7500, 0.8780, 0.1160, 
             0.0010,
             0.1780, 0.0010, 0.0010, 0.0010,
             0.0010, 0.0280, 0.0120,0.3830,
             0.0030, 0.0410, 0.5760, 0.4740,
             0.0140, 0.0410, 0.0040, 0.5190,
             0.0300, 0.0320, 0.7350)

results <- countSignificantTests_Holms(pValues, addComps = 0)
results

temp <- data.frame(pValue <- numeric(0))
temp <- rbind(temp, data.frame(pValue = pValues))
temp$holmsValues <- results$holmsValues

countSignificantTests_Bonferroni(pValues)

dataForGraph <- data.frame(pValueIndex = seq(1:length(results$holmsValues)), holmsValues = results$holmsValues, sigPValues = results$sigPValues)
dataForGraph$isSig <- FALSE
dataForGraph$isSig[results$sigPIndexes] <- TRUE

ggplot(data = data.frame(pValueIndex = seq(1:length(results$holmsValues)), holmsValues = results$holmsValues), aes(x=pValueIndex, y = holmsValues)) +
  geom_line(colour = "red") +
  geom_point()+
  geom_bar(data=data.frame(pValueIndex = seq(1:length(results$sigPValues)), pValues = results$sigPValues), aes(x=pValueIndex, y=pValues), stat="identity", fill="green")

pValues <- c(0.6, 0.4, 0.1, 0.9, 0.2)
names <- c("age", "sex", "income", "race", "treatment-arm")
plotSigP_Holms(names, pValues)
holmsValues = getHolmsPvalues(length(pValues))
results <- countSignificantTests_Holms(pValues)
results
isSig <- seq(1:length(pValues)) %in% results$sigPIndexes
dataForGraph <- data.frame(index = seq(1:length(pValues)), pValues = format(pValues, scientific=FALSE), names, holmsValues, isSig) # Don't know why I have to format those pValues to get them not to print on the screen in scientific notation, but for some reason today I am
ggplot(data=dataForGraph, aes(x=index, y=pValues, fill=isSig))+
  geom_bar(stat="identity") +
  labs(x="Predictor", y="P-values", title=sprintf("You have %d significant p-value%s", sum(dataForGraph$isSig, na.rm=TRUE), ifelse(sum(dataForGraph$isSig, na.rm=TRUE) == 1, "", "s"))) +
  scale_x_reverse(breaks=dataForGraph$index, labels=dataForGraph$names) +
  scale_fill_discrete(name="Significant") +
  coord_flip()

}


###
# Exmples of rejecting a null hypothesis for when the effect is greater than good enough
# and for when the effect is smaller than good enough (non-inferiority)
# This is put into a function just to allow the file to be sourced and such.

# From the example in Serlin and Labsley 1993
exampleFunc <- function(){
  n1 <- 10 # specified size of group 1
  n2 <- 10 # specified size of group 2
  N <- n1+n2 # total sample size
  alpha <- 0.05 # pre-specified alpha level
  goodEnoughDiff <- 0.2 # pre-specified minimum difference considered meaningful specified in units of standard deviations
  sampleSizeAdjustment <- sqrt(n1*n2/N)
  lamda <- sampleSizeAdjustment * goodEnoughDiff # non centrality parameter
  tCrit <- qt(1-alpha, N-2, ncp=lamda)  # critical value required to reject
  
  
  # First example is testing that the effect is GREATER than a minimum meaningful effect (goodEnoughDiff)
  observedEffSize = 1.0 # This is the effect size observed
  tObserved <- sampleSizeAdjustment*observedEffSize # critical value
  
  # Reject if tObserved greater than tCrit
  rejectNull <- tObserved - tCrit > 0
  sprintf("Test indicates to %s the null", ifelse(rejectNull == TRUE, "reject", "retain"))
  
  # If we do reject the null, what is the confidence interval?
  if (rejectNull == TRUE) {
    maxEffSizeOfNoInterest <- getLamdaCI(lamda, tObserved, prob=1-alpha, df=N-2)
    
    sprintf("True effect is > %3.3f", maxEffSizeOfNoInterest)
  }
  
  # Now flip the hypotheses and indicate if we think the effect should be smaller than good enough (this would be non-inferiority)
  observedEffSize = 1.0 # This is the effect size observed
  tObserved <- sampleSizeAdjustment*observedEffSize # critical value
  
  # Reject if tObserved LESS THAN  tCrit
  rejectNull <- tObserved - tCrit < 0
  sprintf("Test indicates to %s the null", ifelse(rejectNull == TRUE, "reject", "retain"))
  
  
  # Now adjust observed effect size to something actually small
  observedEffSize = 0.99 # This is the effect size observed
  tObserved <- sampleSizeAdjustment*observedEffSize # critical value
  
  # Reject if tObserved LESS THAN tCrit
  rejectNull <- tObserved - tCrit < 0
  sprintf("Test indicates to %s the null", ifelse(rejectNull == TRUE, "reject", "retain"))
  
  # If we do reject the null, what is the confidence interval?
  if (rejectNull == TRUE) {
    
    # Note here, prob = alpha, not 1-alpha as in the >= hypothesis
    maxEffSizeOfNoInterest <- getLamdaCI(lamda, tObserved, prob=alpha, df=N-2)
    
    # Note the 'smaller than'
    sprintf("True effect is < %3.3f ", maxEffSizeOfNoInterest)
  }
  
}
