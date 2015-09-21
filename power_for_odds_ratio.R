
pA=(7*.25)/22 # Proportion exposed in group A
pB=(7*.25)/69 # Proportion exposed in group B
kappa=1  # Sampling ratio (how many recruited in group A to group B)
alpha=0.05
beta=0.20

OR <- pA*(1-pB)/pB/(1-pA) # Unadjusted (i.e. univariate odds ratio)


nB <- (1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha/2)+qnorm(1-beta))/log(OR))^2
nB <- ceiling(nB) 
z<- log(OR)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2))

sprintf("OR = %2.3f, Power = %1.3f, size for exp group = %d, size for control group = %d, total n = %d",
        OR, Power, as.integer(nB*kappa), as.integer(nB), as.integer(nB*kappa+nB))