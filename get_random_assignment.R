##
# getRandomIds
# Purpose: Create a list of study ids with indication of random assignment into two conditions (nice feature to add more)
# Arguments:
#   n = An integer argument indicating the total number of participants
#   startId = An integer starting number for Ids, defaults to 1
#   labels = A vector of strings (2), for each level of the random group assignment
# Comments:
# Returns
#   randomIds = a data.frame with two columns of integer values
#     Id = a column of integers, 1 to n
#     Group = a column of integers, (zero or one) indicating group assignment if no "levels" are supplied
#     
##
getRandomIds <- function(n = NULL, startId = 1, labels=NULL){

  if (is.null(n)){
    stop("You called getRandomIds without specifying sample size (n)")
  }
  
  # Sampling is often unbalanced, so we'll just assume n is small and that this will happen
  numSamples <- 100
  samples <- matrix(nrow=numSamples, ncol=n)
  
  for (i in 1:numSamples){
    samples[i,] <- rbinom(n=n, size=1, prob=.5)
  }
 
  # Calculate how far off of a balanced split each sample is
  balance <-  abs(apply(samples, 1, sum) - n/2)
  
  # Get the best balance of 1's and zero's, oting there might be more than one, we'll just take the first one
  bestBalance <- which(balance == min(balance))[1]
  
  randomIds <- data.frame(Id = seq(from=startId, to=startId+n-1), Group = samples[bestBalance,])
  
  if (is.null(labels) == FALSE){
      randomIds$Group <- factor(randomIds$Group, labels=labels)
  }
  return(randomIds)
}

samsIds <- getRandomIds(n=64, startId = 401, labels=c("Walk", "Read"))
melsIds <- getRandomIds(n=64, startId = 501, labels=c("Walk", "Read"))
meredithsIds <- getRandomIds(n=64, startId = 601, labels=c("Walk", "Read"))

write.csv(samsIds, "SamsIds.csv", row.names = FALSE)
write.csv(melsIds, "MelsIds", row.names = FALSE)
write.csv(meredithsIds, "MeredithsIds", row.names = FALSE)
