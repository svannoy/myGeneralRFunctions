library(MASS)
library(ggplot2)

corrdata=function(samples=200,r=0){
  data = mvrnorm(n=samples, mu=c(0, 0), Sigma=matrix(c(1, r, r, 1), nrow=2), empirical=TRUE)
  X = data[, 1]  # standard normal (mu=0, sd=1)
  Y = data[, 2]  # standard normal (mu=0, sd=1)
  data.frame(x=X,y=Y)
}
 
df=data.frame()
for (i in c(1,0.8,0.5,0.2,0,-0.2,-0.5,-0.8,-1)){
  tmp=corrdata(200,i)
  tmp['corr']=i
  df=rbind(df,tmp)
}
 
library(ggplot2)
 
g=ggplot(df,aes(x=x,y=y))+geom_point(size=1)
g+facet_wrap(~corr)+ stat_smooth(method='lm',se=FALSE,color='red') +
 coord_fixed()