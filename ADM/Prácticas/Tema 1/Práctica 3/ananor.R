##Análisis de la normalidad una muestra
ananor<-function(x)	
{
 par(mfrow=c(2,2))
 n<-length(x)
 qqnorm(x,main=paste("Gráfico Normal de Prob. n=",n))
 qqline(x)
 boxplot(x,main="Gráfico de caja y bigotes")
 hist(x,col="red",xlim=c(min(x),max(x)),br=30,freq=FALSE)
 curve(dnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
 add=TRUE,lwd=2)
 plot(ecdf(x), do.points=FALSE, verticals=TRUE)
 curve(pnorm(x,mean(x),sd(x)),min(x),max(x),1000,col="blue",
 add=TRUE,lwd=2)
 par(mfrow=c(1,1)) 
 shapiro.test(x)
}



