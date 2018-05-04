###########################################
#ANALISIS DE DATOS MULTIVARIANTES.        #  
#GRADO EN MATEMÁTICAS.                    #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA #
#ANÁLISIS FACTORIAL: EJEMPLO SEISHU       #
###########################################
#El fichero "Seishu.txt" contiene el valor de 
#10 características para 30 marcas de vino Seishu
#La variable Sake es el llamado 
#"Sake Meter Value", cuanto más alto positivo,
#más seco, cuanto más negativo, más dulce

#1. Leer los datos y calcular las medidas
#   de adecuación muestral. Aplicar el test
#   de esfericidad de Bartlett
#2. Estimar el modelo AF mediante el método 
#   de Componentes Principales (variables tipificadas)  
#3. Estimar el modelo AF mediante el método 
#   de Factores Principales (variables tipificadas) 
#   usando el paquete psych
#4. Estimar el modelo AF mediante el método 
#   de máxima verosimilitud (variables tipificadas) 
#   (función factanal)

#1. Leer los datos y calcular las medidas
#de adecuación muestral. Aplicar el test
#de esfericidad de Bartlett
#########################################
datos= read.table("Seishu.txt",header=FALSE)
colnames(datos)= c("Sabor","Olor","Ph",
                    "Acidez1","Acidez2","Sake",
                    "Azúcar_reducido","Azúcar_Total",
                    "Alcohol", "Formyl-Nitrog.")
datos
summary(datos)

#Medidas de adecuación muestral
#install.packages("rela")
library(rela)
PAF=paf(as.matrix(datos))
#KMO mide la proporción de varianza que es común, 
#atribuible a factores subyacentes
PAF$KMO #Aquí un 55% (Bajo)
PAF$MSA  #Medidas de adecuación muestral

#el test de esfericidad de Bartlett contrasta 
#la hipótesis nula de que la matriz
#var-cov es proporcional a la identidad, 
#en este caso, R=I, implicaría la
#incorrelación entre las variables
PAF$Bartlett #estadístico del test
#cálculo del p-valor
p=ncol(datos)
gl= p*(p-1)/2
1-pchisq(PAF$Bartlett,gl)   #Contraste significativo

#(normalidad multivariante cuestionable
#p.value.small
#[1] 2.904e-05 )


#2. AF mediante el método de 
#   Componentes Principales (R)  
############################
R=cor(datos)
(autoval= eigen(R)$values)       
(autovec= eigen(R)$vectors)
plot(princomp(R))  #Parece sugerir 4
100*sum(autoval[1:4])/10   #84.4%

m= 4  #número de factores
L=autovec[,1:m]%*%diag(sqrt(autoval[1:m]))  #Cargas factoriales
rownames(L)= colnames(datos)     #Coinciden con las correlaciones
colnames(L)= paste("Factor",1:m)  #entre variables y factores (R)
round(L,3)
(h= apply(L^2,1,sum))   #comunalidades
(psi= diag(R)-h)        #varianzas específicas
cbind(L,h,psi)           

#Contribuciones de los factores
contrib=   apply(L^2,2,sum)
contrib   #conciden con autoval[1:m]
resumen= matrix(NA,nrow=m,ncol=3)
resumen[,1]=  contrib[1:m]
resumen[,2]= 100*resumen[,1]/sum(autoval)
resumen[,3]= cumsum(resumen[,2])
colnames(resumen)= c("Varianza","Porcentaje",
                     "Porcentaje acumulado")
resumen

#El siguiente gráfico representa las variables 
#según las cargas factoriales
plot(L,type="n",main="Cargas factoriales",
 xlab="Factor 1",ylab="Factor 2")
text(L,labels=rownames(L),cex=0.8)
grid()
abline(h=0,v=0,lty=2)

#Rotación varimax, simplifica la interpretación
(rotvarimax= varimax(L))
(Lrot= varimax(L)$loadings)
plot(Lrot[,1:2],type="n",
     main="Cargas factoriales, rotación varimax",
     xlab="Factor 1",ylab="Factor 2")
text(Lrot[,1:2],labels=rownames(L),cex=0.8)
grid()
abline(h=0,v=0,lty=2)
(hrot= apply(Lrot^2,1,sum))  #comunalidades
(psirot= diag(R)-hrot)        #varianzas específicas
cbind(L,h,psi,Lrot,hrot,psirot)

#Puntuaciones factoriales para la rotación varimax
#Para la solución de componentes principales 
#se puede usar mínimos cuadrados ordinarios
#Los datos deben ser centrados, 
#o sea, scale(...center=TRUE)
#Como estamos trabajando con la matriz de 
#correlaciones, en realidad las variables 
#han de ser tipificadas, por
#tanto también scale=TRUE
Xcent=scale(datos,center=TRUE,scale=TRUE)
Frot=t(solve(t(Lrot)%*%Lrot)%*%t(Lrot)%*%t(Xcent))
rownames(Frot)= 1:nrow(datos)
Frot

#Nube de puntos de las puntuaciones factoriales
plot(Frot,type="n",main="Puntuaciones factoriales 
     mediante mínimos cuadrados \n Rotación varimax")
text(Frot,labels=rownames(Frot),cex=0.7,col="red")
abline(h=0,v=0,lty=2,col="blue")

plot(Frot[,c(1,3)],type="n",
     main="Puntuaciones factoriales mediante mínimos cuadrados \nRotación varimax")
text(Frot[,c(1,3)],labels=rownames(Frot),cex=0.7,col="red")
abline(h=0,v=0,lty=2,col="blue")

#En general, los pares (Fi,Fj), con i<j

#Aproximación a R
R4=Lrot%*%t(Lrot) + diag(psirot)
R4
R
Rresid= R-R4
sum(Rresid^2)
sum(autoval[-c(1:m)]^2)


#3. Método de Factores  Principales    
#####################################
#install.packages(c("psych","GPArotation"))
library(psych)    #Necesita la librería GPArotation
library(GPArotation)
af.fp = fa(datos,4,fm="pa",rotate="varimax",max.iter=350)
af.fp 
#####################################
#4. Estimar el modelo AF mediante el método 
#   de máxima verosimilitud (variables tipificadas) 
#   (función factanal)
###################################################
af.mv = factanal(datos,m,scores="Bartlett")
#scores="regresssion" para el otro método
af.mv #Ya aplica rotación varimax; contraste satisfactorio
cbind(Comunalidad=1-af.mv$uniqueness,Especificidad=af.mv$uniqueness)
L=loadings(af.mv); L
for (i in 1:(m-1))
 for (j in (i+1):m)
  { plot(L[,c(i,j)],type="n",
         main="Cargas factoriales; Máxima Verosimilitud\nRotación varimax")
      text(L[,c(i,j)],labels=rownames(L),cex=0.7,col="red")
    abline(h=0,v=0,lty=2,col="blue")
  }
#Puntuaciones factoriales; 
F= af.mv$scores
rownames(F)= 1:nrow(datos)
for (i in 1:(m-1))
 for (j in (i+1):m)
  { plot(F[,c(i,j)],type="n",
         main="Puntuaciones factoriales Barlett \nRotación varimax")
      text(F[,c(i,j)],labels=rownames(F),cex=0.7,col="red")
    abline(h=0,v=0,lty=2,col="blue")
  }

