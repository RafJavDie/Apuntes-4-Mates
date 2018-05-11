###########################################
#ANALISIS DE DATOS MULTIVARIANTES.        #  
#GRADO EN MATEMÁTICAS.                    #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA #
#ANÁLISIS FACTORIAL: EJEMPLO DRUGUSE      #
###########################################
#El espacio de trabajo druguse.RData contiene
#el objeto druguse, que es la matriz de correlaciones
#obtenida a paritr de una encuesta sobre 
#el consumo de drogas y alcohol, a la que
#respondieron 1634 estudiantes en centros educativos 
#de Los Ángeles
#1. Cargar en R el espacio de trabajo. 
#   gráficamente el objeto druguse
#2. Estimar el modelo AF mediante máxima verosimilitud
#   seleccionando el número de factores a partir
#   del procedimiento de bondad de ajuste
#3. Estimar el modelo AF mediante 
#   el método de Factores Principales 

load(file="druguse.RData")  #Matriz de correlaciones
druguse

library(corrplot)
corrplot(druguse,method="ellipse")

#2. Estimar el modelo AF mediante máxima verosimilitud
#   seleccionando el número de factores a partir
#   del procedimiento de bondad de ajuste
######################################################
#Determinar el número de factores según p-valores
pvalores<-sapply(1:7, function(nf)
  factanal(covmat = druguse, factors = nf, 
           method = "mle", n.obs = 1634)$PVAL)
names(pvalores)<- 1:7
round(pvalores,4)  #Sugiere 6 factores
plot(eigen(druguse)$values,type="h")

#AF máxima verosimilitud con 6 factores
af.mv<-factanal(covmat = druguse, factors = 6,  
                method = "mle", n.obs = 1634)
af.mv       #Rotación varimax (por defecto)
#Factor 1: drogas suaves/sociales: cigarettes, beer, wine, liquor, marijuana 
#Factor 2:Cocaine, tranquillizers, heroin: drogas duras 
#Factor 3: uso de amphetamine
#Factor 4: hashish 
#Los factores 5 y 6 pueden no ser relevantes

#diferencias entre correlaciones observadas y reproducidas
 pfun <- function(nf) {
     fa <- factanal(covmat = druguse, factors = nf, 
                    method = "mle", n.obs = 1634)
     est <- tcrossprod(fa$loadings) + diag(fa$uniquenesses)
     ret <- round(druguse - est, 3)
     colnames(ret) <- rownames(ret) <- 
         abbreviate(rownames(ret), 3)
     ret
 }

 pfun(6)
 summary(pfun(6))   #con 6 hay un buen ajuste
 summary(pfun(3))
 boxplot(pfun(6),ylim=c(-0.1,0.1),col="red",
   main="Correlaciones Residuales\n Método de máxima verosimilitud" )
 boxplot(pfun(3),add=TRUE,col="lightblue")
 legend("topleft",col=c("red","lightblue"),pch=4,
        legend=c("6 Factores","3 Factores"))
 
 
 #3. Método de Factores Principales
 ##################################
 library(psych)   
 library(GPArotation)
 af.fp <- fa(druguse,6,n.obs=1634,fm="pa",rotate="none")
 af.fp             #Poco interpretable
 plot(af.fp)
 fa.diagram(af.fp) 
 
 #rotación varimax
 af.fp <- fa(druguse,6,n.obs=1634,fm="pa",rotate="varimax")
 af.fp             #Mejor
 plot(af.fp)
 fa.diagram(af.fp) 
  
 #Rotación oblicua:
 af.fp <- fa(druguse,6,n.obs=1634,fm="pa")
 af.fp
 plot(af.fp)
 fa.diagram(af.fp)  
 
 
 