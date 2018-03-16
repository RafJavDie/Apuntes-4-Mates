##########################################
#ANALISIS DE DATOS MULTIVARIANTES.       #  
#GRADO EN MATEMÁTICAS.                   #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA#
#HOJA DE PROBLEMAS 5.                    #
#ANALISIS DE CONGLOMERADOS               #
##########################################

#1. LECHE.sav
#############
#1.i). LEER LOS DATOS Y RESUMIRLOS
library(foreign)
mamiferos<- data.frame(read.spss("leche.sav"))
dim(mamiferos)
head(mamiferos)
row.names(mamiferos)<- mamiferos$animal
mamiferos<- mamiferos[,-1]
mamiferos
summary(mamiferos)

#1.ii)
pairs(mamiferos)  
cor(mamiferos)   

#1.iii). DISTANCIAS Y hclust
D<- dist(mamiferos)
round(D,1)
conglo<- hclust(D,method="complete")
plot(conglo, cex=0.8,labels=rownames(mamiferos),
     main="Dendrograma",xlab="Completo",
     sub="Distancias",ylab="Distancias",col="blue")

cbind(conglo$merge,conglo$height) 
#Lista de uniones que se van produciendo. Se una el de la primera columna con el del segundo y la distancia en la tercera. -n representa el caso n, en positivo representa un conglomerado. Por ejemplo, -1 1 indica que el 1 se une al conglomerado 1, donde (en este caso) están el 2 y el 9.

#1. iv)
cluster <- cutree(conglo, k = 3)
table(cluster)
split(mamiferos,cluster)
#LO MISMO CON OTRO FORMATO:
by(mamiferos,cluster,function(x) x) 

#Calcular los centroides:
Centros=by(mamiferos,cluster,colMeans)  
#(FORMATO list)
Centros= t(sapply(split(mamiferos,cluster),colMeans))  
#(FORMATO matrix)

#1. v) 
#ESTADISTICO F DEL ANOVA de 1 FACTOR. Agua y grasa
apply(mamiferos,2, function(x) 
  summary(lm(x~factor(cluster)))$fstatistic[1])
colores<- c("red","blue","green")
attach(mamiferos)
plot(agua,grasa,type="n",
     main="Resultado tres conglomerados")
text(agua,grasa,labels=row.names(mamiferos),
     cex=0.6,col=colores[cluster])
text(Centros[,1],Centros[,3],
     labels=row.names(Centros),cex=1.5,col=colores)
grid()

#1.vi)
conglosin<- hclust(D,method="single")
plot(conglosin, cex=0.8,
     labels=rownames(mamiferos),main=conglosin$method,
     xlab="",sub="",ylab="Distancias",col="blue")

congloaver<- hclust(D,method="average")
congloavers<- hclust(D,method="single")

plot(congloaver, cex=0.8,
     labels=rownames(mamiferos),main=congloaver$method,
     xlab="",sub="",ylab="Distancias",col="blue")
plot(congloavers, cex=0.8,
     labels=rownames(mamiferos),main=congloaver$method,
     xlab="",sub="",ylab="Distancias",col="blue")

clustersin <- cutree(conglosin, k = 3)
clusteraver <- cutree(congloaver, k = 3)
table(clustersin)
table(clusteraver)
table(cluster)

table(clusteraver,cluster) #coinciden
table(clusteraver,clustersin)


#1.vii. Con agnes
library(cluster)
agnclus <- agnes(mamiferos, metric = "euclidean", 
                 stand = FALSE,method="complete")
summary(agnclus)
plot(agnclus,
     main=paste("Agnes:",agnclus$method,sep=""))
clusteragn=cutree(agnclus,3)
table(cluster,clusteragn)


#2. USO DEL COEFICIENTE DE GOWER
################################
#2.i.
library(cluster)
data(flower)
flower
?flower

D <- daisy(flower,type = list(asymm = c("V1", "V3"), symm= 2,ordratio= 7,logratio= 8))
D

#2.ii
conglosin<- hclust(D,method="single")
plot(conglosin, cex=0.8,
     main=conglosin$method,ylab="Distancias",
     col="blue",sub="")
conglocom<- hclust(D)
plot(conglocom, cex=0.8,
     main=conglocom$method,ylab="Distancias",
     col="blue",sub="")
cbind(conglocom$merge,conglocom$height) #Lista de uniones que se van produciendo

cluster<- cutree(conglocom,3)
cluster
table(cluster)

split(flower,cluster)
#O BIEN:
which(cluster==1)
which(cluster==2)
which(cluster==3)

#2.iii
agnclusin <- agnes(D,method="single")
summary(agnclusin)
agnclusin$ac

agncluscom <- agnes(D,method="complete")
summary(agncluscom)
agncluscom$ac
plot(agncluscom,main=agncluscom$method)


#3.DATOS swiss
##############
#3.i. k-medias con centros iniciales resultantes 
#de hclust
data(swiss)
?swiss
h<- hclust(dist(swiss),method="complete")
plot(h)
cluster<- cutree(h,3)
Centros= t(sapply(split(swiss,cluster),colMeans))  
#FORMATO matrix
Centros

km<- kmeans(swiss,Centros)   
km
table(km$cluster)


#3.ii. Dibujo sobre 2 variables
apply(swiss,2, function(x) 
  summary(lm(x~factor(km$cluster)))$fstatistic[1])

plot(swiss[,c(5,2)],type="n",
     main="Swiss, KM")
text(swiss[,c(5,2)],labels=row.names(swiss),
     cex=0.6,col=colores[km$cluster])
text(km$centers[,5],km$centers[,2],
     labels=row.names(Centros),cex=1.5,col=colores)
grid()

#3.iii. k-medias desde 15 soluciones iniciales
km3 <- kmeans(swiss, 3, nstart = 15)
km3

table(km3$cluster)

table(km$cluster,km3$cluster)
#(solo cambia la numeración)

#4. EJEMPLO DE COMPRESION DE IMAGENES CON K-MEDIAS
##################################################
#4.i
install.packages('pixmap')
install.packages('jpeg')
library(jpeg)
library(pixmap)
x <- readJPEG("mandril.jpg")
str(x)
#es un array tri-dimensional, o sea, 3 matrices
#(intensidades Red, Green, Blue)
z <- pixmapRGB(c(x[,,1],x[,,2],x[,,3]), 512,512,
               bbox=c(-1,-1,1,1))
plot(z)
R<- x[,,1]
G<- x[,,2]
B<- x[,,3]
datos<- cbind(R=c(R),G=c(G),B=c(B))
head(datos)

#4.ii
for (k in c(2,5,10))
{ km<- kmeans(datos,k)
  datoscom<- km$centers[km$cluster,]
  Rcom<- matrix(datoscom[,1],nrow(R),ncol(R))
  Gcom<- matrix(datoscom[,2],nrow(G),ncol(G))
  Bcom<- matrix(datoscom[,3],nrow(B),ncol(B))
  imagen <- pixmapRGB(c(Rcom,Gcom,Bcom),nrow(R),ncol(R))
  ECM<- mean((datos-datoscom)^2)
  plot(imagen,main=paste("k= ",k),
       xlab=paste("ECM=",round(ECM,4)))
}


#5. DATOS agriculture
#####################
#5.i
library(cluster)
data(agriculture)
?agriculture

plot(agriculture,type="n",main="Casos y medoides")
text(agriculture,labels=row.names(agriculture))
grid()

#5.ii
agric.pam<- pam(agriculture,2)

summary(agric.pam)
plot(agric.pam)

#Nube de puntos, los medioides con mayor tamaño
colores=c("red","blue")
plot(agriculture,type="n",main="Casos y medoides")
text(agriculture[-agric.pam$id.med,],
     labels=row.names(agriculture)[-agric.pam$id.med],
     col=colores[agric.pam$clustering[-agric.pam$id.med]])
grid()
text(agric.pam$medoids,col=c("red","blue"),
     label=rownames(agric.pam$medoids),cex=2)

#5.iii
listak<- 2:7
s<- numeric(length(listak))
for (i in 1:length(listak))
{
   agric.pam.i<- pam(agriculture,listak[i])
   s[i]<-   agric.pam.i$silinfo$avg.width 
}
s
max(s) 
listak[which.max(s)]
plot(listak,s,type="l",xlab="k",
     main="k-medioides",ylab="Silueta media")



#6. DATOS FICTICIOS ruspini
###########################
data(ruspini)
plot(ruspini)

plot(2:10,sapply(2:10,function(x) pam(ruspini,x)$silinfo$avg.width ),type="l")

rusp.pam<- pam(ruspini,4)
plot(rusp.pam)
plot(ruspini,main="4-medioides",
     col=rusp.pam$clustering)
points(rusp.pam$medoids,pch=10,cex=2.5,col="red")
grid()

#7. FUNCION clara
#################
data(xclara)
plot(xclara)
clx3 <- clara(xclara, 3)
plot(clx3)

#Dibujar la nube de puntos distinguiendo
#el conglomerado mediante colores
plot(xclara, col=clx3$clustering)

#8. METODO FANNY
################
agric.fan<- fanny(agriculture,2)
summary(agric.fan)
plot(agric.fan)
plot(agriculture, col=agric.fan$clustering,
     pch=agric.fan$clustering)

#9.MIXTURAS DE NORMALES MULTIVARIANTES
######################################
library(mclust)
data(faithful)
?faithful
plot(faithful)
Mclusfaith=Mclust(faithful) #BUSCAR EL MEJOR MODELO
Mclusfaith
matz<-Mclusfaith$z
round(matz[1:10,],4) #Para cada caso, prob. de pertenencia a cada grupo
plot(Mclusfaith) #0 para terminar

names(Mclusfaith) 
#lista de toda la informacion presente en EMfaith
L<-Mclusfaith$loglik
cat("\nEl valor del log. func. de verosimilitud calculada con los datos producidos
por el algoritmo EM es",L,"\n")
EMpar<-Mclusfaith$parameters
names(EMpar) #estimaciones finales de los parámetros

medias<-EMpar$mean
medias
names(EMpar$variance)
var<-EMpar$variance$sigma
prob<-EMpar$pro #el vector de los pesos de cada componente (sum(prob)=1)

#RESUMEN DE LOS PARAMETROS CALCULADOS CON EL 
#ALGORITMO EM
cat("\n PRIMERA COMPONENTE NORMAL BIVARIANTE:\n",
    "PI(1)=",prob[1],"\n",
    "mu(1)=(",medias[1,1],",",medias[2,1],")\n\n",
    "\n SEGUNDA COMPONENTE NORMAL BIVARIANTE:\n",
    "PI(2)=",prob[2],"\n",
    "mu(2)=(",medias[1,2],",",medias[2,2],")\n\n",
    "\n TERCERA COMPONENTE NORMAL BIVARIANTE:\n",
    "PI(3)=",prob[3],"\n",
    "mu(3)=(",medias[1,3],",",medias[2,3],")\n\n")
        
cat("\n La matriz de covarianzas es la misma para todas las componentes siendo
el modelo de tipo EEE y es:\n\n")
var[,,1]


#10. DATOS wreath
#################
data(wreath)
plot(wreath) #14 componentes
?wreath
grid()
wreathBIC<- mclustBIC(wreath) #sólo resultados BIC
plot(wreathBIC,legendArgs=list(x="topleft"))
#Conviene extender la búsqueda del mejor K
wreathBIC<- mclustBIC(wreath,G=1:20,x=wreathBIC)
plot(wreathBIC,G=10:20,legendArgs=list(x="bottomright"))
summary(wreathBIC,data=wreath)
plot(wreath,
     col=summary(wreathBIC,data=wreath)$classification)
