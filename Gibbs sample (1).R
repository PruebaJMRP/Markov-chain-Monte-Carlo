x=c(3.2,-1.5) # observations
mu0=seq(-10,10,length=20) # starting values
muk=rep(0,250)
par(mfrow=c(5,4),mar=c(4,2,4,1)) # multiple histograms
for (i in 1:20){
  for (t in 1:250){
    mu=mu0[i]
    for (iter in 1:1000){
      omega = rexp(2,1+(x-mu)^2)
      mu = rnorm(1,sum(x*omega)/sum(omega+.05),
           sqrt(1/(.05+2*sum(omega))))
    }
    muk[t]=mu
  }
  hist(muk,proba=T,col="wheat",main=paste(mu0[i]))
}