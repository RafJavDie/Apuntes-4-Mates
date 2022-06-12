#Simulación de mixturas de normales multivariantes
#Según los modelos de Mclust
library(mvtnorm)
n<-400   #Tamaño de las muestras a generar

mu1<- c(-5,-5)
mu2<- c(5,5)
landa<-3
landa1<-3
landa2<-1
I<- diag(2)
A<- diag(c(6,2))
A1<- diag(c(5,2))
A2<- diag(c(2,8))

#EII
x<-rmvnorm(n,mean=mu1,sigma=I)
y<- rmvnorm(n,mean=mu2,sigma=I)
xy<- rbind(x,y)
plot(xy,main="EII",cex.main=2,xlab="x1",ylab="x2")
grid()

#VII
x<-rmvnorm(n,mean=mu1,sigma=landa1*I)
y<- rmvnorm(n,mean=mu2,sigma=landa2*I)
xy<- rbind(x,y)
plot(xy,main="VII",cex.main=2,xlab="x1",ylab="x2")
grid()

#EEI
x<-rmvnorm(n,mean=mu1,sigma=A)
y<- rmvnorm(n,mean=mu2,sigma=A)
xy<- rbind(x,y)
plot(xy,main="EEI",cex.main=2,xlab="x1",ylab="x2")
grid()

#VEI
x<-rmvnorm(n,mean=mu1,sigma=landa1*A)
y<- rmvnorm(n,mean=mu2,sigma=landa2*A)
xy<- rbind(x,y)
plot(xy,main="VEI",cex.main=2,xlab="x1",ylab="x2")
grid()

#EVI
x<-rmvnorm(n,mean=mu1,sigma=A1)
y<- rmvnorm(n,mean=mu2,sigma=A2)
xy<- rbind(x,y)
plot(xy,main="EVI",cex.main=2,xlab="x1",ylab="x2")
grid()

#VVI
x<-rmvnorm(n,mean=mu1,sigma=landa1*A1)
y<- rmvnorm(n,mean=mu2,sigma=landa2*A2)
xy<- rbind(x,y)
plot(xy,main="VVI",cex.main=2,xlab="x1",ylab="x2")
grid()

#EEE

sigma1<- matrix(c(1,-0.7,-0.7,1),2,2)
sigma1
eigen(sigma1)
x<-rmvnorm(n,mean=mu1,sigma=sigma1)
y<- rmvnorm(n,mean=mu2,sigma=sigma1)
xy<- rbind(x,y)
plot(xy,main="EEE",cex.main=2,xlab="x1",ylab="x2")
grid()

#EEV
sigma1<- matrix(c(1,-0.6,-0.6,1),2,2)
sigma2<- matrix(c(1,0.6,0.6,1),2,2)
eigen(sigma1)
eigen(sigma2)

x<-rmvnorm(n,mean=mu1,sigma=sigma1)
y<- rmvnorm(n,mean=mu2,sigma=sigma2)
xy<- rbind(x,y)
plot(xy,main="EEV",cex.main=2,xlab="x1",ylab="x2")
grid()

#VEV
sigma1<- matrix(c(1,-0.6,-0.6,1),2,2)
sigma2<- matrix(c(1,0.6,0.6,1),2,2)
eigen(sigma1)
eigen(sigma2)

x<-rmvnorm(n,mean=mu1,sigma=landa1*sigma1)
y<- rmvnorm(n,mean=mu2,sigma=5*landa2*sigma2)
xy<- rbind(x,y)
plot(xy,main="VEV",cex.main=2,xlab="x1",ylab="x2")
grid()

#VVV
sigma1<- matrix(c(1,-0.8,-0.8,1),2,2)
sigma2<- matrix(c(1,0.3,0.3,1),2,2)
eigen(sigma1)
eigen(sigma2)

x<-rmvnorm(n,mean=mu1,sigma=landa1*sigma1)
y<- rmvnorm(n,mean=mu2,sigma=5*landa2*sigma2)
xy<- rbind(x,y)
plot(xy,main="VVV",cex.main=2,xlab="x1",ylab="x2")
grid()
