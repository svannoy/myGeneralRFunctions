
my.chi.test.ind <- function(x) {
  if (is.null(dim(x)) == FALSE){
    if (dim(x)[1] != 2 | dim(x)[2] != 2) {
    stop("Called chi.test.ind with something other than a 2 by 2 table")
    }
}
else {
    stop("Called chi.test.ind with something other than a 2 by 2 table")
  }
  nC1 <- as.numeric(sum(x[1:2]))
  nC2 <- as.numeric(sum(x[3:4]))
  nR1 <- as.numeric(x[1] + x[3])
  nR2 <- as.numeric(x[2] + x[4])
  n <- as.numeric(sum(x[1:4]))
  myChiSquare <- (n*(x[1]*x[4] - x[3]*x[2])^2)/(nC1*nC2*nR1*nR2)
  myP <- 1-pchisq(myChiSquare,1)
  sprintf("Chi-Square = %3.7f, df =1 p = %3.7f", myChiSquare, myP)
  return(data.frame(chiSquare = myChiSquare, p = myP))
}

x <- c(1, 3, 4, 5, 6)
my.chi.test.ind(x)

# This example matches Rand Wilcox's example on page 742
x=matrix(c(8,5,67,20),ncol=2,byrow=T)
my.chi.test.ind(x)

# Note it is, as he says different from what you get in chisq.test
chisq.test(as.table(x)) # This gives an error, I think because of the cell sizes

# But again, matches what you get from the summary on a table)
summary(as.table(x))

x2 <- as.table(matrix(c(355, 260, 289, 103), ncol=2, byrow=F))
chisq.test(x2)
summary(x2)
my.chi.test.ind(x2)