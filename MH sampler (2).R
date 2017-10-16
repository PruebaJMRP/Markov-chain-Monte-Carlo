#Ejercicio 4.1
bank <- read.table("C:/Users/alabarca/Desktop/bank.txt", quote="\"")
bank=matrix(bank,byrow=T,ncol=5)
y=as.vector(bank[,5])
X=cbind(rep(1,200),as.vector(bank[,1]),as.vector(bank[,2]),
        as.vector(bank[,3]),as.vector(bank[,4]))
summary(lm(y~X[,5]))

#Ejercicio 4.6
target=function(x,the1=1.5,the2=2){
  x^(-the1)*exp(-the1*x-the2/x)
}
al=sqrt(4/3)*0.5
bet=1/5
# valor inicial
mcmc=rep(1,1000)
for (t in 2:1000){
  y = rgamma(1,shape=al,rate=bet)
  if (runif(1)<target(y)*dgamma(mcmc[t-1],shape=al,rate=bet)/
      (target(mcmc[t-1])*dgamma(y,shape=al,rate=bet)))
    mcmc[t]=y
  else
    mcmc[t]=mcmc[t-1]
}
# plots
par(mfrow=c(2,1),mar=c(4,2,2,1))
res=hist(mcmc,freq=F,nclass=55,prob=T,col="grey56",
         ylab="",main="")
plot(mcmc,type="l",col="steelblue2",lwd=2)

#Ejercicio 4.7
al=4.3
bet=6.2
mcmc=matrix(1,ncol=1000,nrow=500)
for (t in 2:1000){
  mcmc[,t]=mcmc[,t-1]
  y = rgamma(500,4,rate=7)
  valid=(runif(500)<dgamma(y,al,rate=bet)*
           dgamma(mcmc[,t-1],4,rate=7)/(dgamma(mcmc[,t-1],al,rate=bet)*
                                           dgamma(y,4,rate=7)))
  mcmc[valid,t]=y[valid]
}
aver2=apply(mcmc,1,cumsum)
aver2=t(aver2/(1:1000))
ranj2=apply(aver2,2,range)
plot(ranj2[1,],type="l",ylim=range(ranj2),ylab="")
polygon(c(1:1000,1000:1),c(ranj2[2,],rev(ranj2[1,])))

#Ejercicio 4.8

x=c(0,5,9)
# target
targ=function(y){
  dnorm(y,sd=sqrt(50))*dt(y-x[1],df=1)*
    dt(y-x[2],df=1)*dt(y-x[3],df=1)
}
# Chequeo tri-modalidad
plot(seq(-2,15,length=250),
     targ(seq(-2,15,length=250)),type="l")
sigma=c(.001,.05,1)*9 # different scales
N=100000 # number of mcmc iterations
mcmc=matrix(mean(x),ncol=3,nrow=N)
for (t in 2:N){
  mcmc[t,]=mcmc[t-1,]
  y=rcauchy(3,0,sigma) 
  valid=(runif(3)<targ(y)/targ(mcmc[t-1,]))
  mcmc[t,valid]=y[valid]
}
m1=mean(mcmc[,1])
m2=mean(mcmc[,2])
m3=mean(mcmc[,3])

