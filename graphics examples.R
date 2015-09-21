install.packages("ggthemes")
install.packages("gridExtra")
install.packages("compute.es")
install.packages("MBESS")

library(compute.es)
library(MBESS)
library('reshape2')
library('ggplot2')
library(gridExtra)
library(ggthemes)

stuff<-data.frame(
  people=c(rep("Klingon",9), rep("Vulcan",9),rep("Cardassian", 14)),
                  food=c(rep("Breakfast",3),rep("Lunch",4),rep("Dinner",6),rep("Breakfast",7),rep("Lunch",3),rep("Dinner",5),rep("Lunch",4)))

p <- ggplot(stuff, aes(factor(people), fill=food))
p + geom_bar() + 
stat_bin(aes(label=paste("n = ",..count..)), vjust=1, geom="text")

# I followed some examples on stack overflow and tried to use reshape2 and ggplot2 to do this plot:

rs = data.frame(seq(200, 1000, by=200), runif(5), runif(5), runif(5))
    names(rs)=c("time", 1:3)


# I first melt the data into a "long-format":

    melted = melt(rs, id.vars="time")
    ggplot() + geom_line(data=melted, aes(x=time, y=value, group=variable))


tea=c("Arabian","French Roast")
Sales=c(10000, 15000)
Plan=c(12000,12000)
Variance=c(-2000,3000)
df=data.frame(tea=tea,Sales=Sales,Plan=Plan,Variance=Variance)


salesplan=ggplot(df,aes(x=tea,y=Sales,fill=tea))+geom_bar(stat="identity")+
  geom_segment(aes(x=as.numeric(df$tea)-.475,xend=as.numeric(df$tea)+.475,y=df$Plan,yend=df$Plan))+
  ggtitle("Sales vs Plan")+ coord_flip() +theme_few()+scale_fill_few("medium")+
  theme(legend.position="None", axis.title.y=element_blank(),axis.title.x=element_blank())

varianceplan=ggplot(df,aes(x=tea,y=Variance,fill=tea))+geom_bar(stat="identity")+
  coord_flip()+ggtitle("Variance to Plan")+theme_few()+scale_fill_few("medium")+
  theme(legend.position="None",axis.text.y = element_blank(),axis.title.y=element_blank(),
        axis.title.x=element_blank())

grid.arrange(salesplan,varianceplan,ncol=2)


( m <- matrix(1:12, 3, 4) )
div.3 <- m %% 3 == 0
which(div.3)
which(div.3, arr.ind = TRUE)
rownames(m) <- paste("Case", 1:3, sep = "_")
which(m %% 5 == 0, arr.ind = TRUE)


library(psych)
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged


data(galton)
galton.tab <- table(galton)
galton.tab[order(rank(rownames(galton.tab)),decreasing=TRUE),] #sort it by decreasing row values

str(galton.tab)
rcorr(galton$child,galton$parent)
cor.test(galton$child,galton$parent)
cov(galton$child,galton$parent)

lm(galton$parent ~ galton$child)
galFix = lm(galton$child ~ galton$parent)
summary(galFix)
pairs(galton$child ~ galton$parent)




# 
library(MESS)
library(MBESS)
library(ggplot2)
library(dplyr)
library(compute.es)

# The following code demonstrates the need for increased sample size in order
# to get the same width of a given confidence interval as the effect size increases
# from Kristoffer Magnusson's blog about R, STATISTICS, PSYCHOLOGY, OPEN SCIENCE, DATA VISUALIZATION
# http://rpsychologist.com

ss2 <- NULL
# nested loops to run ss.aie.smd with different deltas and widths
for(j in seq(0.2, 1, by=0.2)) {
  ss <- NULL
  for(i in seq(0.1,2,by=0.2)) {
    ss <- c(ss, ss.aipe.smd(delta=i, width=j))
  }
  if(j == 0.2) {
    ss2 <- data.frame("n" = ss)
    ss2$width <- j
  } else
  {
    ss_tmp <- data.frame("n" = ss)
    ss_tmp$width <- j
    ss2 <- rbind(ss2, ss_tmp)
  }
}
ss2$delta <- rep(seq(0.1,2,by=0.2), times=5) # add deltas used in loop
ggplot(ss2, aes(delta, n, group=factor(width), linetype=factor(width))) + geom_line()



# The following code was created while investigating the nature of confidence intervals for Cohen's d
# It appears that the "compute.es" function "mes" uses an incorrect algorithm for computing the effect size confidence interval

# This first section just uses the MBESS function "ss.aipe.smd" to get the necessary sample size needed
# to have a confidence interval of width=.5, it confirms code above, not sure why I duplicated it here
j <- 1
k <- NULL
x <- NULL
for (i in seq(.1,1, by=.1)){

x[j] <- ss.aipe.smd(delta=i, width=.5)
k <- c(k, ss.aipe.smd(delta=i, width=.5))
j <- j+1
}



library(MBESS)
library(ggplot2)
library(dplyr)
library(compute.es)

# The section of code computes a series of confidence intervals for a specific standardized mean difference (cohen's d) and increasing sample sizes
# The CI's are computed using two different functions from two different packages: ci.smd from MBESS and des from compute.des
# The results from each method are compiled in separate dataframes in a format meant to facilitate comparision


# Set the standardized mean difference (aka Cohen's d) 
test.smd <- .8

# Loop over a sequence of increasing sample sizes, starting relatively small, going to moderate
ciFromMBESS <- NULL
ciFromCompute.es <- NULL

for (i in seq(25, 400, by=25)){
  
  # Get confidence interval data from the given SMD and sample size
  x <- ci.smd(smd=test.smd, n.1=i, n.2=i)
  
  # Collect CI data for each sample size into a data.frame
  ciFromMBESS <- rbind(ciFromMBESS,data.frame( smd=x$smd,                       # Store the SMD just for clarity, using value returned just for verification
                                               sample.size=i+i,                 # Total sample size is 2*i
                                               lci=x$Lower.Conf.Limit.smd,
                                               uci=x$Upper.Conf.Limit.smd,
                                               lci_width = x$smd-x$Lower.Conf.Limit.smd, 
                                               uci_width=x$Upper.Conf.Limit.smd-x$smd, 
                                               assymetry=(x$smd - x$Lower.Conf.Limit.smd) - (x$Upper.Conf.Limit.smd - x$smd), 
                                               total_width=x$Upper.Conf.Limit.smd-x$Lower.Conf.Limit.smd))
  
    # Get confidence interval data from the given SMD and sample size
  x2 <- des(d=test.smd, n.1=i, n.2=i, dig=10, verbose=FALSE)
 
  # Collect CI data for each sample size into a data.frame
  ciFromCompute.es <- rbind(ciFromCompute.es,data.frame( smd=test.smd,
                                                         sample.size=i+i, 
                                                         lci=x2$l.d, 
                                                         uci=x2$u.d, 
                                                         lci_width = test.smd-x2$l.d, 
                                                         uci_width=x2$u.d-test.smd, 
                                                         assymetry=(test.smd-x2$l.d)-(uci_width=x2$u.d-test.smd), 
                                                         total_width=x2$u.d-x2$l.d))


}

# Print out the respective calculations from each package
print(ciFromMBESS)
print(ciFromCompute.es)

# Compute difference in overall width from two methods as an absolute value and then as a percentage of the CI width from the MBESS package
compareCIWidth <- data.frame(MBESS_Width=ciFromMBESS$total_width, CompES_Width = ciFromCompute.es$total_width, difference = ciFromCompute.es$total_width - ciFromMBESS$total_width)
compareCIWidth$percentDiff = compareCIWidth$difference/compareCIWidth$MBESS_Width

# Print out the difference in width of CI from compute.es and MBESS"
print(compareCIWidth)



n1 <- 1000
n2 <- 1000
x2 <- NULL
ciMbess <- NULL
myParams <- NULL
nTot <- (n1+n2)
for (i in seq(.1, 1, by=.1)){
  
  x2 <- ci.smd(smd=i, n.1=n1, n.2=n2)
  
  myParams <- data.frame(nTot=nTot, d=i, lci=x2[[1]],uci=x2[[3]], width=x2[[3]]-x2[[1]], 
                         width_lci = i-x2[[1]], width_uci=x2[[3]]-i, asymmetry = ((i-x2[[1]])-(x2[[3]]-i)) )
  if (i == .1){
    ciMbess <- myParams
  }
  else{
    ciMbess <- rbind(ciMbess, myParams)
  }
}

ciMbess



n1 <- 1000
n2 <- 1000
x3 <- NULL
ciCompEs <- NULL
ciCompEsG <- NULL
myParams <- NULL
myParamsG <- NULL
nTot <- (n1+n2)
for (i in seq(.1, 1, by=.1)){
  
  x3 <- des(d=i, n.1=n1, n.2=n2, dig=10, verbose=FALSE)
  cat(x3$g,"\n")
  myParams <- data.frame(nTot=nTot, d=x3$d, lci=x3$l.d,uci=x3$u.d, width=x3$u.d-x3$l.d, 
                         width_lci = i-x3$l.d, width_uci=x3$u.d-i, asymmetry = ((i-x3$l.d)-(x3$u.d-i)) )
  myParamsG <- data.frame(nTot=nTot, d=x3$g, lci=x3$l.g,uci=x3$u.g, width=x3$u.g-x3$l.g, 
                         width_lci = i-x3$l.g, width_uci=x3$u.g-i, asymmetry = ((i-x3$l.g)-(x3$u.g-i)) )
  if (i == .1){
    ciCompEs <- myParams
    ciCompEsG <- myParamsG
  }
  else{
    ciCompEs <- rbind(ciCompEs, myParams)
    ciCompEsG <- rbind(ciCompEsG, myParamsG)

  }
}

ciCompEs



a = c(0.40000000, 0.44011976, 0.72727273, 0.50000000, 0.00000000, 0.07692308, 0.00000000, 0.00000000, 0.00000000, 1.00000000, 0.50000000, 0.91666667, 0.19354839, 0.74883721, 0.50000000, 0.50000000, 0.55000000, 0.17142857, 0.50000000, 0.51351351, 0.68000000, 0.85714286, 0.03703704, 0.05454545, 0.54219949, 0.44444444, 0.00000000, 0.00000000)
b = c(0.00000000, 0.54491018, 0.72727273, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, 1.00000000, 0.00000000, 0.00000000, 0.00000000, 0.33953488, 0.00000000, 0.00000000, 0.00000000, 0.48571429, 0.00000000, 0.83783784, 0.80000000, 0.57142857, 0.00000000, 0.00000000, 0.06393862, 0.90476190, 0.00000000, 0.00000000)
mean(a)
sd(a)
mean(b)
sd(b)
t.test(a, b, paired=TRUE)

library(compute.es)
mes(mean(a), mean(b), sd(a), sd(b), length(a), length(b), dig=4)

# Now we'll repeat the above simulation, x times
numReps=10
effects <- vector("list", numReps)
effects2 <- vector("list", numReps)
baseN = 200
baseMean=17
postN=200
postMean=11
for( i in 1:numReps){
baseline <- rnorm(baseN, baseMean)
post <- rnorm(postN, postMean)
effects[[i]] <- cohd(baseline,post)
cat((effects[[i]][[1]]-effects[[i]][[4]]) - (effects[[i]][[5]]-effects[[i]][[1]]))
cat("\t")
effects2[[i]] <- ci.smd(smd=effects[[i]][[1]], n.1=baseN, n.2=postN)
cat((effects2[[i]][[2]]-effects2[[i]][[1]]) - (effects2[[i]][[3]]-effects2[[i]][[2]]))
cat("\n")

}

set.seed(0)
x <- sample(0:9, 100, rep=T) 
n <- length(x)          # number of observations
r <- order(order(x))    # order of values, i.e. ranks without averaged ties
p <- (r - 1/2) / n      # assign to ranks using Blom's method
y <- qnorm(p)           # theoretical standard normal quantiles for p values
plot(x, y)              # plot empirical against theoretical values
