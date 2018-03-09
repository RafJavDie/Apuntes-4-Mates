#############################################
#Análisis de Datos Multivariantes.          #
#Grado en Matemáticas.                      #
#Doble Grado en Matemáticas y Estadística   #
#Hoja 4. Prácticas de Inferencia            #
#Multivariante con R                        #
#############################################


##1.Datos pulmonary
###################
#1.i
####
install.packages("ICSNP")
library(ICSNP)
data(pulmonary)
?pulmonary
summary(pulmonary)
str(pulmonary)
cor(pulmonary)
cov(pulmonary)
pairs(pulmonary)

#1.ii
#####
#Estudio de la normalidad
apply(pulmonary,2,shapiro.test)
#Test de Mardia
source("Test_Mardia.R")
mardiaTest(pulmonary)

#1.iii
######
#Contraste sobre la media, la siguiente función 
#es de ICNSP
#El valor T.2=6.6202 es directamente el valor de 
#la transformación al estadístico F
HotellingsT2(pulmonary,mu = c(0,0,2))   
#Haciendo los cálculos directamente
print(n<- nrow(pulmonary))
print(Sigmag<- cov(pulmonary))
print(Sinv<- solve(Sigmag))
mu0<- c(0,0,2)
(media<- apply(pulmonary,2,mean))
(dife<- rbind(media-mu0))
(T2<- n*dife%*%Sinv%*%t(dife))
(F<- (n-3)/(3*(n-1))*T2)

#Los tests univariantes:
t.test(pulmonary[,1],mu=0)
t.test(pulmonary[,2],mu=0)
t.test(pulmonary[,3],mu=2)


#2.Región de confianza para mu
##############################
#2.i
######
library(mvtnorm)
set.seed(13579)
n<- 20
x<- rmvnorm(n,c(0,0),matrix(c(1,0.6,0.6,1),2,2))
plot(x,col="red",xlab="x1",ylab="x2",
     xlim=c(-3.5,3.5),ylim=c(-3.5,3.5),
     main="Una muestra de una ley Normal bivariante \n Regiones de Confianza")
media<- apply(x,2,mean)
points(media[1],media[2],col="red",lwd=6)
grid()

#2.ii
#####
install.packages("ellipse")
library(ellipse)
dt=apply(x,2,sd)
lines(ellipse(cor(x), scale = dt, centre = media, 
              level = 0.99),lwd=2,lty=2,col="orange")
lines(ellipse(cor(x), scale = dt, centre = media,
              level = 0.95),lwd=2,lty=2,col="blue")
lines(ellipse(cor(x), scale = dt, centre = media, level = 0.90),
      lwd=2,lty=2,col="green")
legend("topleft",col=c("orange","blue","green"),
       lty=2,lwd=2,legend=c("99%","95%","90%"))

#3. Dos medias
##############
#3.i
####

set.seed(12345)
library(mvtnorm)
n=20
m=30
X <- rmvnorm(n, c(0, 0, 0, 0), diag(1:4))
Y <- rmvnorm(m, c(0.5, 0.5, 0.5, 0.5), diag(1:4))
Z <- rbind(X, Y)
g <- factor(rep(c(1,2),c(20,30)))
data.frame(Z,g)
summary(Z[g==1,])
summary(Z[g==2,])
#Se puede obtener con by:
by(Z, g,summary)

medias=by(Z,g,function(M)  apply(M,2,mean) ); medias
sigmags= by(Z,g, cov ); sigmags
str(sigmags)     #Estructura tipo lista
sigmags[[1]]     #Acceso a una componente
pairs(Z,col=c("red","blue")[g])

#3.ii
#####
source("Test_Mardia.R")
by(Z,g,mardiaTest)

( D2Maha= by(Z,g, function(M) mahalanobis(M,apply(M,2,mean),cov(M))) )


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
  grid()  #Alguna evidencia de no linealidad y outlier
}
QQBeta(D2Maha[[1]],n,4)
QQBeta(D2Maha[[2]],m,4)


#Test M de Box para comparar las matrices var-cov
#Es muy sensible. Si las muestras son del 
#mismo tamaño,se recomienda ignorarlo. 
#Si no, rechazar cuando p<0.001
MBox<- function(X,g)
{
 ni<- table(g)
 n<- sum(ni)
 ng<- length(ni)
 vdet<- numeric(ng)
 p<- ncol(X)
 Sigma<- matrix(0,p,p)
 for (i in 1:ng)
  {
  sigmai<- cov(X[g==levels(g)[i],])
  vdet[i]<- det(sigmai)
  Sigma<-    Sigma+ (ni[i]-1)*sigmai
  }
 Sigma<- Sigma/(n-ng)
 M<-(n-ng)*log(det(Sigma))-sum((ni-1)*log(vdet))
 f1<- (ng-1)*p*(p+1)/2
 pho<- 1-((2*p^2+3*p-1)/(6*(p+1)*(ng-1))*(sum(1/(ni-1))-(1/(n-ng))))
 tau<- -((p-1)*(p+2))/(6*(ng-1))*(sum(1/((ni-1)^2))-(1/((n-ng)^2)))
 f2<- (f1+2)/abs(tau-(1-pho)^2)
 gamma<- (pho-f1/f2)/f1
 pvalor=pf(gamma*M,f1,f2,lower.tail=FALSE)
 names(M)<- names(pvalor)<- NULL
 return(list(M=M,pvalor=pvalor,gl1=f1,gl2=f2))
}
MBox(Z,g)

#3.iii
######
library(ICSNP)
#HotellingsT2(Z ~ g)  #contraste de igualdad de medias
HotellingsT2(Z ~ g, mu = rep(-0.5,4))

#Si falla alguna de las hipótesis se puede 
#recurrir a un contraste no paramétrico:
install.packages("cramer")
library(cramer)  
#Un test no paramétrico para dos poblaciones
#cramer.test(Z[g==1,],Z[g==2,],sim="permutation")  
#Contraste de igualdad de medias
cramer.test(Z[g==1,]-rep(-0.5,4),Z[g==2,],sim="permutation")  
#H0: diferencia de medias=(-0.5,-0.5,-0.5,-0.5)

#4. Otro ejemplo de dos poblaciones
#Se quiere ver la efectividad de un tratamiento 
#para niños con problemas de rendimiento escolar
#Puntulec:Puntuación en un test de lectura
#Puntuari:Puntuación en un test de aritmética
#4.i
####
datos<- read.table("Rendiescolar.dat")
colnames(datos)<- c("Grupo","Probrend",
                    "Puntulec","Puntuari","CI")
datos
datos$Grupo<- factor(datos$Grupo)
levels(datos$Grupo)<- c("Tratamiento","Control")
datos$Probrend<- factor(datos$Probrend)
levels(datos$Probrend)<- c("Débil","Moderado","Severo")
datos

#4.ii
#####
Z<- as.matrix(datos[,3:4])
g<- datos$Grupo
plot(Z,col=c("red","blue")[g])
legend("topleft",legend=levels(g),
       col=c("red","blue"),pch=1)
grid()

#Calcular centroides (se puede hacer con by, 
#como en el ejercicio anterior)
mediaT<- apply(Z[g=="Tratamiento",],2,mean)
mediaC<- apply(Z[g=="Control",],2,mean)
points(mediaT[1],mediaT[2],pch=6,col="red",lwd=4)
points(mediaC[1],mediaC[2],pch=6,col="blue",lwd=4)

#4.iii
######
source("Test_Mardia.R")
by(Z,g,mardiaTest)   
HotellingsT2(Z ~ g, mu = c(0,0))  
#Tamaños muestrales iguales (no aplicamos MBox)

#5. Datos de los pintores
#########################
#5.i
####
library(MASS)
data(painters)
summary(painters)
?painters
str(painters)

#Vamos a seleccionar las categorías A y D
datos<- painters[painters$School=="A" |
                   painters$School=="D",]
datos$School<- factor(datos$School)  
#(Para olvidar el resto de escuelas)
levels(datos$School)<- c("Renacimiento","Veneciana")
summary(datos)
attach(datos)

#5.ii
#####
#Gráficos caja y bigotes
colores<- c("red","blue")
p<- ncol(datos)-1
par(mfrow=c(2,2))
for (i in 1:p)
 boxplot(datos[,i]~datos[,p+1],col=colores,main=names(datos)[i])
par(mfrow=c(1,1))

##5.iii
######Nubes de puntos
pairs(datos[,-5],col=colores[School])

#Una nube de puntos
plot(Composition,Drawing, type="n")
text(Composition,Drawing, label=rownames(datos),
     cex=0.8,col=colores[School])
legend("topleft",col=colores,
       legend=levels(School),pch=0)

#Las seis nubes de puntos
par(mfrow=c(2,3))
for (i in 1:(p-1))
  for (j in (i+1):p)
  {
     plot(datos[,c(i,j)], type="n")
     text(datos[,c(i,j)], label=rownames(datos),
          cex=0.9,col=colores[School])
     grid()
  }
par(mfrow=c(1,1))


#5.iv
######
#Análisis de la normalidad univariante
for (i in 1:p)
{
 cat(names(datos)[i],"\n")
 cat("============","\n") 
 print(lapply(split(datos[,i],datos[,p+1]),
              shapiro.test))
}
#Igualdad de Varianzas
for (i in 1:p)
{
 cat(names(datos)[i],"\n")
 cat("============","\n") 
 print(var.test(datos[,i]~datos[,p+1]))
}

#Comparación de medias
for (i in 1:p)
{
 cat(names(datos)[i],"\n")
 cat("============","\n") 
 print(t.test(datos[,i]~datos[,p+1],
              var.equal=var.test(datos[,i]~datos[,p+1])$p.value>0.05))
}
 
#5.v
#####
#Contrastes multivariantes
source("Test_Mardia.R")
by(datos[,-5],datos[,5],mardiaTest)

#MBox(datos[,1:p],datos[,p+1]) 
#Seguir la recomendación para n iguales
library(ICSNP)
HotellingsT2(as.matrix(datos[,1:p]) ~ datos[,p+1],
             mu = rep(0,p))


#6. MANOVA. Con 3 grupos
########################
#6.i
####
data(iris)
iris
str(iris)
#Estudio de la normalidad multivariante
#Gráficamente: pares de nubes de puntos, 
#forma elíptica o circular
iris.color<-c("red","green","blue")[iris$Species]
pairs(iris[,-5],col=iris.color)


#Estudio de la normalidad univariante para 
#cada variable y grupo
#lapply(split(iris[,1:4],iris$Species),apply,2,shapiro.test)
#Test de Mardia
source("Test_Mardia.R")
by(iris[,-5],iris[,5],mardiaTest)


#De todos modos, el TCL multivariante puede 
#utilizarse para garantizar la normalidad multivariante 
#de las medias, 
#si bien se recomienda para ello
#la regla "n>10p"

#6.ii
#####
modelo<-manova(as.matrix(iris[,-5])~iris[,5])
summary.aov(modelo)         #ANOVA univariante
summary(modelo,test="Pillai")
summary(modelo,test="Wilks")
summary(modelo,test="Hotelling-Lawley")
summary(modelo,test="Roy")

#7. Test para la simetría
#########################
#NIVEL=Nivel socioeconómico familiar(alto, medio-bajo)
#LECTUPRI=Test de lectura en primavera
#LECTUOTO=Test de lectura tras el verano
#ARITPRI=Test de aritmética en primavera
#ARITOTO=Test de aritmética tras verano
#7.i
####
#para mantener el orden de las transparencias
datos<- read.table(file="Simetria.dat",header=TRUE)
datos<- datos[,c(1,2,4,3,5)] 
datos
X<- datos[,-1]
X
matplot(X,type="l",col=c("red","blue","red","blue")
        ,lty=c(2,2,1,1),xlab="Caso")
legend("bottomleft",col=c("red","blue","red","blue"),
       lty=c(2,2,1,1), legend=colnames(X))
grid()

#7.ii
#####

#dibujar las diferencias

A<- cbind(diag(2),-diag(2))
A
Ax<- A%*%t(as.matrix(X))
Ax
Ax<- t(Ax)
matplot(Ax,type="l",col=c("red","blue"),
        lwd=2,xlab="Caso")
legend("bottomleft",col=c("red","blue"),lwd=2,
 legend=c("Dif.Lectura","Dif.Aritmética"))
grid()

#7.iii
######
library(ICSNP)
#mardiaTest(datos)  
mardiaTest(Ax)
HotellingsT2(Ax,mu = c(0,0))

#7.iv
#####
#Para ver si las diferencias medias coinciden 
#en los dos grupos
datos$Nivel=factor(datos$Nivel)
plot(Ax,col=c("red","blue")[datos$Nivel]) 
legend("bottomright",col=c("red","blue"),
       legend=c("Nivel alto","Nivel bajo"),pch=1)   
grid()
by(Ax,datos$Nivel,mardiaTest)
HotellingsT2(Ax ~ factor(datos$Nivel))


#8. Medidas repetidas, un solo grupo
####################################
#8.i
####
datosNC<- read.table(file="InflamabilidadNoCab.txt",header=T)
matplot(t(datosNC),type="l",xlab="Mes",ylab="Inflamb.",
        main="Sin pastar las cabras")
tiempo<- 1:ncol(datosNC)
mediaNC<- apply(datosNC,2,mean)
lines(mediaNC,lwd=4,col="red")
grid()

#8.ii
#####
#Según las transparencias, necesitamos
# C   [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    0    0    0    0   -1
#[2,]    0    1    0    0    0   -1
#[3,]    0    0    1    0    0   -1
#[4,]    0    0    0    1    0   -1
#[5,]    0    0    0    0    1   -1
p<- ncol(datosNC)
C<- cbind(diag(p-1),cbind(rep(-1,p-1)))
C
Cx<- C%*%t(as.matrix(datosNC))
Cx
Cx<- t(Cx)
library(ICSNP)
mardiaTest(Cx)
HotellingsT2(Cx,mu = rep(0,p-1)) 


#9. Análisis de perfiles
########################
#9.i
####
datosC<- read.table(file="InflamabilidadCab.txt",header=T)
matplot(t(datosC),type="l",xlab="Mes",
        ylab="Inflamb.",main="Pastando las cabras")
mediaC<- apply(datosC,2,mean)
lines(tiempo,mediaC,lwd=4,col="blue")
grid()

plot(tiempo,mediaC,lwd=2,col="blue",type="l",
     ylim=c(9000,19000),ylab="Media")
lines(tiempo,mediaNC,lwd=2,col="red")
legend("topleft",col=c("blue","red"),lty=1,lwd=2,
 legend=c("Pastando","Sin pastar"))
grid()

#9.ii
##### 

A<- matrix(0,p-1,p)
for (i in 1:(p-1))
  A[i,c(i,i+1)]<-c(-1,1)
A

XNoc<-  t(A%*%t(as.matrix(datosNC)))
XC<-  t(A%*%t(as.matrix(datosC)))
XNoc
XC
mardiaTest(XNoc)
mardiaTest(XC)
grupo<- factor(  c(rep(1,nrow(datosNC)),
                   rep(2,nrow(datosC))) )
levels(grupo)=c("Sin pastar","Pastando")
MBox(rbind(XNoc,XC),grupo)
HotellingsT2(rbind(XNoc,XC) ~ grupo)  
#Se rechaza el paralelismo

#10. Aquí se acepta el paralelismo y 
# luego se comparan los perfiles
#####################################
datos<- read.table("Paralelismo.txt",header=T)
datos$Grupo=factor(datos$Grupo)
p<-ncol(datos)-1
tiempo<- 1:p
medias=by(datos[,-1],datos[,1],function(M)  
  apply(M,2,mean) ); medias

matplot(t(datos[,-1]),type="l",col=c("red","blue")[datos$Grupo],
        lty=c(1:2)[datos$Grupo],
        main="Perfiles de los dos grupos, incluyendo los perfiles medios",
        xlab="Mes",ylab="")
lines(tiempo,medias[[1]],lwd=5,lty=1,col="red")
lines(tiempo,medias[[2]],lwd=5,lty=2,col="blue")
legend("topleft",col=c("red","blue"),lty=1:2,legend=c("Grupo 1","Grupo 2"))
grid()

A<- matrix(0,p-1,p)
for (i in 1:(p-1))
  A[i,c(i,i+1)]<-c(-1,1)
A
Xtrans<-  t(A%*%t(as.matrix(datos[,-1])))
by(Xtrans,datos$Grupo,mardiaTest)
MBox(Xtrans,datos$Grupo)
library(ICSNP)
HotellingsT2(Xtrans ~datos$Grupo)  
#Se acepta el paralelismo


#¿Son iguales los perfiles medios?
B<- matrix(1,nrow=1,ncol=p)
XtransB<-  t(B%*%t(as.matrix(datos[,-1])))
cbind(XtransB,datos$Grupo)
sapply(split(XtransB,datos$Grupo),shapiro.test)
var.test(XtransB~datos$Grupo)  
t.test(XtransB ~ factor(datos$Grupo),var.equal=FALSE) #Se rechaza



