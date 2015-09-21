



fit <- lm(gad7 ~ phq8, data = ard)
summary(fit)
intercept = coef(fit)[1]
slope = coef(fit)[2]

myP = ggplot(ard, aes(x=phq8, y=gad7)) + geom_point() + 
  stat_smooth(method="lm")
myP + 
# don't need this, the smooth function above adds a line, just playing with plotting a line where I want it
myP + geom_abline(intercept=coef(fit)[1], slope=coef(fit)[2], colour="red")
