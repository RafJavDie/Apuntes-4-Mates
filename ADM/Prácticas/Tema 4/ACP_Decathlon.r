################################################
#ANALISIS DE DATOS MULTIVARIANTES.             #  
#GRADO EN MATEMÁTICAS                          #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA      #
#ANALISIS DE COMPONENTES PRINCIPALES           #
#DATOS DECATHLON                               #
################################################

########################
#1.Análisis descriptivo
########################
library(foreign)
decath<- data.frame(read.spss("decathlon1989.sav"))
summary(decath)
boxplot(decath[,1:10]) #mejor trabajar con las correlaciones
R<- cor(decath[,1:10]) #matriz de correlaciones
round(R,2)
det(R)  #valor pequeño, señal de alto grado 
#de intercorrelación

########################################
#2.Análisis de componentes principales
########################################

acp<- princomp(decath[,1:10], cor = TRUE)
str(acp)   #estructura resultante
summary(acp)  #Da las d.t. de las componentes

#Un resumen con mejor formato:
resumen<- matrix(NA,nrow=length(acp$sdev),ncol=3)
resumen[,1]<-  acp$sdev^2
resumen[,2]<- 100*resumen[,1]/sum(resumen[,1])
resumen[,3]<- cumsum(resumen[,2])
colnames(resumen)<- c("Autovalor","Porcentaje",
                      "Porcentaje acumulado")
resumen

eigen(R)$values  #Coinciden con resumen[,1]

#Gráfico de sedimentación
plot(resumen[,1],type="h",
     main="Datos decathlon 1989",ylab="Autovalor")
abline(h=mean(resumen[,1]),lwd=2,lty=2,col="blue")
#otra opción:
plot(acp,col="blue",main="Decathlon 1989") 




#####################################################
#3. Un contraste de hipótesis sobre el número de C.P.
#####################################################
#Requiere normalidad multivariante
#Test de Mardia:
#p.value.kurt   : 0.8166246 
#p.value.small  : 0.1051081 

#n: número de casos
#p: número de variables
#landa: vector de autovalores
#H0: landa[m+1]=...=landa[p]=0
numcp<- function(n,p,m,landa)
{  a<-n- ((2+p+11)/6)
  lmedia<- log(mean(landa[(m+1):p]))
  suma<- sum(log(landa[(m+1):p]))
  Q<- a*( (p-m)*lmedia-suma)
  gl<- (p-m+2)*(p-m+1)/2
  pvalor<- 1-pchisq(Q,gl)
  list(Q=Q,gl=gl,pv=pvalor)
}
cbind(0:9,t(sapply(0:9,numcp,n=nrow(decath),p=10,acp$sdev^2)))  
#Sugiere dos C.P.
#Se acepta #H0: landa[3]=...=landa[p]=0
#Sin embargo las dos primeras CP sólo explican
#el 71% de la varianza total

#####################################################
#4. Coeficientes y correlaciones de las C.P.
#####################################################

loadings(acp) #Coeficientes que definen cada combinación lineal
eigen(R)$vectors  #Coinciden
#para calcular las correlaciones entre las 
#variables y las componentes
cor_vc<-loadings(acp)%*%diag(acp$sdev)
cor_vc
cor_vc[,1:2]  #las dos primeras
#También se pueden calcular mediante
#cor(decath[,1:10],acp$scores)[,1:2]


#Para ayudar a interpretar las CP:
plot(cor_vc[,1:2],type="n",
     main="Decathlon 1989",
     xlab="C.P. 1",ylab="C.P.2")
text(cor_vc[,1:2],labels=rownames(cor_vc),
     col="red",cex=0.6)
abline(h=0,v=0,lty=1,col="blue")
abline(v=0.5,lty=2)
abline(v=-0.5,lty=2)
abline(h=-0.5,lty=2)

#Comunalidades: para cada variable, es 
#el coeficiente R2 entre esa variable
#y el conjunto de las c.p.seleccionadas
#Se puede calcular como la suma de las 
#correlaciones al cuadrado con las 
#c.p. seleccionadas
cbind(apply(cor_vc[,1:2]^2,1,sum)) 

#Se comprueba que la variable altura
#queda poco explicada con solo dos CP
#Mejor con 3 CP:
cbind(apply(cor_vc[,1:3]^2,1,sum)) 

#Con todas las CP la comunalidad es 1:
cbind(apply(cor_vc^2,1,sum))  

#Correlaciones reproducidas con dos c.p.
#Descomposición espectral:
descompespec<-eigen(R)
autovalores<- descompespec$values
autovectores<- descompespec$vectors
autovectores%*%diag(autovalores)%*%t(autovectores)
R
#Aproximación mediante las dos primeras CP:
Raprox2<- autovectores[,1:2]%*%diag(autovalores[1:2])%*%t(autovectores[,1:2])
Raprox2  #Matriz de correlaciones reproducidas

#Con 3 CP:
Raprox3<- autovectores[,1:3]%*%diag(autovalores[1:3])%*%t(autovectores[,1:3])
Raprox3  #Matriz de correlaciones reproducidas

#Residuales:
Res2=R-Raprox2
Res3=R-Raprox3
round(Res2,2)
round(Res3,2)
mean(Res2^2)
mean(Res3^3)

#La diagonal de Res contiene 1 menos la comunalidad
cbind(diag(Res3),1-apply(cor_vc[,1:3]^2,1,sum))



#####################################################
#5. Representación gráfica de los resultados
#####################################################
#representación de los casos mediante
#las puntuaciones de las CP
plot(acp$scores[,1:2],type="n",
     main="Decathlon 1989",
     xlab="C.P. 1",ylab="C.P.2")
text(acp$scores[,1:2],labels=decath$nompais,cex=0.6)
abline(h=0,v=0,lty=2,col="blue")

