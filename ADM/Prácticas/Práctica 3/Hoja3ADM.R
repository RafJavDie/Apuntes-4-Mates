###########################################
#Análisis de Datos Multivariantes         #
#Grado en Matemáticas                     #
#Doble Grado en Matemáticas y Estadística #
#HOJA 3                                   #
###########################################

#1. Normal univariante. 
#######################
#Funciones dnorm, pnorm, qnorm
curve(dnorm(x),-4,4,1000,lwd=2,col="blue",
      main="Función de densidad de la ley N(0,1)",
      ylab="f(x)")
grid()

curve(pnorm(x),-4,4,1000,lwd=2,col="blue",
      main="Función de distribución de la ley N(0,1)",
      ylab="F(x)")
grid()


curve(dnorm(x,10),6,19,1000,lwd=2,col="blue",
      main="Función de densidad de las leyes N(10,1),N(15,1)",ylab="f(x)")
curve(dnorm(x,15),6,19,1000,lwd=2,col="red",add=TRUE)
grid()

curve(dnorm(x,15,1),6,23,1000,lwd=2,col="blue",
      main="Función de densidad de las leyes N(15,1),N(15,4)",ylab="f(x)")
curve(dnorm(x,15,2),6,23,1000,lwd=2,col="red",add=TRUE)
grid()

qnorm(0.975)   #Cuantiles de la ley normal
qnorm(0.025)
curve(dnorm(x),-4,4,1000,lwd=2,col="blue",
      main="Función de densidad de la ley N(0,1)",ylab="f(x)")
abline(v=qnorm(0.975),col="red")
abline(v=qnorm(0.025),col="red")
grid()

pnorm(qnorm(0.975))
pnorm(qnorm(0.025),lower.tail = FALSE)
qnorm(0.975,mean=0,sd=2)


#2. Normal univariante. Bondad de ajuste 
########################################
source("ananor.r")
x=rnorm(100,mean=5,sd=2)
ananor(x)
x=rexp(100,1)
ananor(x)

#3. Algunas inferencias univariantes
####################################
#3.1. Una muestra de 15 empresas, se 
#mide el gasto en publicidad (miles de euros)
#durante el pasado año. Calcular un I.C. 95%
#para el gasto medio y realizar
#un contraste de hipótesis bilateral 
#H0:E[X]=15, H1:E[X]!=15
x<-c(17,12,15,16,15,11,12,13,20,16,14,13,11,10,13)
summary(x)
ananor(x)    #Se acepta la normalidad
#i)
t.test(x)    #IC y contraste H:E[X]=0
#ii) El IC incluye al valor 15, se 
#aceptaría H0 en un contraste bilateral
#O bien
t.test(x,mu=15)    #Se acepta H0
#Ilustración gráfica
resul<-  t.test(x,mu=15); str(resul)
curve(dt(x,resul$parameter),-4,4,1000,main="R.C. test-t",lwd=2,
      ylab=paste("Densidad t-Student ",resul$parameter,"g.l."),xlab="x")
abline(h=0)
abline(v=qt(0.975,resul$parameter),col="red",lty=2)
abline(v=qt(0.025,resul$parameter),col="red",lty=2)
abline(v=resul$statistic,col="blue",lwd=2)
grid()
legend("topleft",lty=2:1,col=c("red","blue"),
       legend=c("Cuantiles 0.025 y 0.975",
                "Estadístico obs."),lwd=2:1)

#3.2. Dos muestras independientes
#Tiempos de recuperación con cierta medicina (x) 
#y grupo placebo (y)
#H0:E[X]=E[Y], H1:E[X]<E[Y]
x <- c(15, 10, 13, 7, 9, 8, 21, 9, 14, 8)
y <- c(15, 14, 12, 8, 14, 7, 16, 10, 15, 12)
ananor(x)
ananor(y)
boxplot(x,y,names=c("Medicamento","Placebo"))
var.test(x,y)      #test F
#Se acepta la igualdad de varianzas
t.test(x,y,alt="less",var.equal=TRUE)

#3.3. Ahora las varianzas son distintas
x <- c(11, 10, 8, 8, 10, 7, 12, 8, 11, 8)
y<- c(15, 10, 13, 7, 9, 8, 21, 9, 14, 8)
ananor(x)
ananor(y)
boxplot(x,y,names=c("Medicamento","Placebo"))
var.test(x,y)      #test F, es significativo
t.test(x,y,alt="less")  #Por defecto var.equal=FALSE


#4. Cómo son las nubes de puntos de 
# muestras de normales bivariantes
###################################
library(mvtnorm)
n<-500   #Tamaño de las muestras a generar

par(mfrow=c(3,3))
for (rho in c(0,0.1,0.2,0.3,0.4,0.5,0.7,0.9,1))
{
matriz<-array(c(1,rho,rho,1),c(2,2))
#Poniendo -rho se tiene corr.neg.

x<-rmvnorm(n,mean=c(0,0),sigma=matriz)
plot(x,main=paste("Normal 2- d,rho=-",rho),col="blue", xlab="X", ylab="Y")
}
par(mfrow=c(1,1))


#5. Dibujo de la función de densidad de 
#la normal bivariante
#######################################
#a) Dibujando la "campana"
library(mvtnorm)
x<-c(-30:30)/10
y<-c(-30:30)/10
media<-c(0,0)

for (rho in c(0,0.5,-0.5,0.7,-0.7,0.9,-0.9))
{
matriz<-array(c(1,rho,rho,1),c(2,2))
nx<-length(x)
ny<-length(y)
z<-array(0,c(nx,ny))

for (i in 1:nx)
 for (j in 1:ny)
  z[i,j]<-dmvnorm(c(x[i],y[j]),mean=media,sigma=matriz)
 persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",
  main= paste("Correlación =",rho), xlab="X",ylab="Y", zlab="f.d.")
}

#b) Dibujando los contornos de densidad constante
for (rho in c(0,0.5,-0.5,0.7,-0.7,0.9,-0.9))
{
 matriz<-array(c(1,rho,rho,1),c(2,2))
 nx<-length(x)
 ny<-length(y)
 z<-array(0,c(nx,ny))

 for (i in 1:nx)
  for (j in 1:ny)
   z[i,j]<-dmvnorm(c(x[i],y[j]),mean=media,sigma=matriz)
 image(z,col=topo.colors(50),xlab="x",ylab="y",
     main= paste("Correlación =",rho)) 
 #Más claro cuanto mayor la f. densidad
  contour(z,add=TRUE,drawlabels=T,nlevels=20)
} #siguiente rho



#6.Estudio de la normalidad multivariante
#########################################
#i) Generar una muestra

source("ananor.r")   
library(MASS)   
#MASS: Para usar mvrnorm, otra opción para generar 
#muestras de la Normal mult.
set.seed(12345)
n<-80
mu<-c(1,3,8,10,4)
SIGMA<-array(c(2,0.4,0.4,0.3,0.5, 0.4,1,-0.2,0.5,0.7,0.4,-0.2, 1, 0.6, -0.2,
0.3, 0.5, 0.6, 1, 0.3, 0.5, 0.7, -0.2, 0.3, 1),
 c(5,5))

x<-mvrnorm(n,mu=mu,Sigma=SIGMA); x

media<-apply(x,2,mean)
cbind(media,mu)

#ii) Estudio de la normalidad multivariante

apply(x,2,ananor)  #Estudio de la normalidad de cada componente

pairs(x,col="red") #Análisis gráfico de la Normalidad bivariante de cada par

sigmag<- var(x)
D2Maha<- mahalanobis(x,media,sigmag)   
 #D^2 = (x - media)' sigmag^-1 (x - media)
plot(D2Maha,type="h")

#Gráfico de cuantiles con la chi-cuadrado
p<- ncol(x)
n<- nrow(x)

QQchisq<- function(D2,n,p)
{
  D2ord<- sort(D2)
  qi<- qchisq(((1:n)-0.5)/n,p)
  plot(D2ord,qi,col="red",xlab=paste(expression(D^2),"observado"),
  ylab="Cuantiles",main=paste("Q-Q, chi-cuadrado ", p,"g.l."))
  abline(a=0,b=1,lty=2,lwd=2,col="blue")
  grid()

}
QQchisq(D2Maha,n,p)


#Con la Beta
QQBeta<- function(D2,n,p)
{
  i<- 1:n
  ui<- sort(n*D2/((n-1)^2))
  a<- p/2
  b<- (n-p-1)/2
  alfa<- (p-2)/(2*p)
  beta<-  (n-p-3)/ (2*(n-p-1))
  vi<- qbeta((i-alfa)/(n-alfa-beta+1),a,b)
  plot(ui,vi,col="red",xlab="u_i observados",
    ylab="v_i",main=paste("Q-Q Beta","a=",a,"b=",b))
  abline(a=0,b=1,lty=2,lwd=2,col="blue")
  grid()  
}

QQBeta(D2Maha,n,p)


#Test de normalidad multivariante: Mardia
install.packages("MVN")
library(MVN)
mnv(x)

#Otros dos tests de normalidad multivariante
#install.packages("mvShapiroTest")
#library(mvShapiroTest)
#mvShapiro.Test(x)
#install.packages("mvnormtest")
#library(mvnormtest)
#mshapiro.test(t(x))

#iii)Identificación de outliers multivariantes

ui<- n*D2Maha/((n-1)^2)
Fi<- ((n-p-1)/p)*(1/(1-ui)-1)
Fn<- max(Fi)

pvalormaxF<- function(f,n,p)
{
  1-pf(f,p,n-p-1)^n
}
pvalormaxF(Fn,n,p)
plot(Fi,type="h")
which.max(Fi)


#7.Análisis de un conjunto de datos
###################################
#El siguiente fichero contiene para 
#10 localizaciones:
#x1: nivel de calcio en el suelo (miliequivalentes/100g)
#x2: cantidad de calcio intercambiable
#x3: calcio en berenjena
datos<- read.table("Calcio.txt",header=TRUE)
datos
datos<- datos[,-1]
pairs(datos)
media<- apply(datos,2,mean)
sigmad<- var(datos)
D2Maha<- mahalanobis(datos,media,sigmad)   
#Gráfico de cuantiles
p<- ncol(datos)
n<- nrow(datos)
QQchisq(D2Maha,n,p)
QQBeta(D2Maha,n,p)  #¿Outlier?
library(MVN)
mvn(datos)      #aquí conviene p.value.small

ui<- n*D2Maha/((n-1)^2)
Fi<- ((n-p-1)/p)*(1/(1-ui)-1)
Fn<- max(Fi)
pvalormaxF(Fn,n,p)  #Se identifica como outlier
plot(Fi,type="h")
which.max(Fi)
mvn(datos[-3,])  #Ahora se acepta la normalidad con más claridad

