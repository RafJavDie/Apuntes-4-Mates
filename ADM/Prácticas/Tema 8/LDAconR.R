##########################################
#ANÁLISIS DE DATOS MULTIVARIANTES
#EJEMPLOS DE ANÁLISIS DISCRIMINANTE CON R
#########################################

#Para el test de Mardia:
source("Test_Mardia.r")
#Para el test M de Box
source("MBox.r")

#################################################
#1. Datos sobre el límite de elasticidad y 
#resistencia final para piezas de acero fabricadas
#considerando dos temperaturas
#################################################
#######
#(a)
#######
datos<- read.table(file="Acero.txt",header=TRUE)
datos
datos[,1]<- factor(datos[,1]) 
#Para que sea tratada como factor
summary(datos)

plot(datos[,2:3],col=datos[,1],lwd=2)
grid()

#Calcular vectores de medias
medias=by(datos[,2:3],datos[,1],colMeans)
names(medias)
media1= medias$"1"
media2= medias$"2"
points(media1[1],media1[2],col=1,lwd=4,pch=2)
points(media2[1],media2[2],col=2,lwd=4,pch=2)

segments(media1[1],media1[2],media2[1],media2[2],
         col="blue",lty=2,lwd=2)

#Normalidad multivariante

by(datos[,2:3],datos[,1],Test_Mardia)

#Contraste de igualdad de matrices de dispersión

MBox(datos[,2:3],datos[,1])
           
#Aquí se aceptan las hipótesis de normalidad 
#multiv. y homocedasticidad
library(ICSNP)
HotellingsT2(as.matrix(datos[,2:3]) ~ datos[,1])

#######
#(b)
#######
library(MASS)
anadis<-lda(datos[,-1],datos[,1])
boxplot(predict(anadis)$x~datos[,1],ylab="FLD") 
#Valores altos de la FLD: Temperatura 2
anadis #a mayor Lim_elast y menor Resisten_Fin 
#más probable Temp. 2
plot(anadis)      #Clara separación de ambos grupos
#FLD en cada grupo
predict(anadis)$x[datos$Temperatura==1]
predict(anadis)$x[datos$Temperatura==2]
           
#En la función lda, los coeficientes de la 
#FLD están normalizados como sigue: 
(a<-anadis$scaling)   #Coeficientes FLD  
#LOs coeficientes que devuelve lda verifican que
#  t(a) %*% Sigmagorro %*% a = 1


#Calculemos el vector landa gorro 
#que aparece en las transparencias 
S1<-cov(datos[datos$Temperatura==1,2:3])
S2<-cov(datos[datos$Temperatura==2,2:3])
ni<- table(datos$Temperatura)
n<- sum(ni)
pigorro<- ni/n
Sigmagorro<- ((ni[1]-1)*S1+(ni[2]-1)*S2)/(n-2)
landag=solve(Sigmagorro)%*%cbind(media1-media2)   
data.frame(landag=landag,lda=a)
landag/a #Son iguales salvo constante

# La constante resultante es -la distancia de Mahalanobis entre los centros:

sqrt(t(cbind(media2-media1))%*%solve(Sigmagorro)%*%cbind(media2-media1))

# Comprobamos que t(a) %*% Sigmagorro %*% a = 1
t(a)%*% Sigmagorro%*%a

# Así, las coordenadas (en este caso, dimensión 1) resultantes 
# por lda() de los puntos tienen como unidad
# la distancia de Mahalanobis entre centros

#################################################
#2. Datos sobre n=732 desplazamientos al trabajo
#################################################
#######
#(a)
#######
datos<- read.table(file="Desplaz.txt",header=TRUE)
summary(datos)
colores=c("red","blue")
pairs(datos[,1:3],col= colores[datos[,4]],
      main="Automóvil=Rojo")

par(mfrow=c(1,3))
boxplot(datos[,1]~datos[,4],main=names(datos)[1],
        col=colores)
boxplot(datos[,2]~datos[,4],main=names(datos)[2],
        col=colores)
boxplot(datos[,3]~datos[,4],main=names(datos)[3],
        col=colores)
par(mfrow=c(1,1))

#Normalidad multivariante (se acepta)

Test_Mardia(datos[datos$tipo=="auto",1:3])
Test_Mardia(datos[datos$tipo=="bus",1:3])
#Homocedasticidad:
MBox(datos[,1:3],datos[,4])

#Contraste sobre la igualdad de medias
library(ICSNP)
HotellingsT2(as.matrix(datos[,1:3])~datos[,4])

#######
#(b)
#######
#lda
library(MASS)
anadis<-lda(datos[,-4],datos[,4])
anadis

boxplot(predict(anadis)$x~datos[,4])  
#Valores altos de la FLD: bus

#A mayor nivel de ingresos, menor coste relativo
#y menor tiempo de espera, mayor es la tendencia a 
#elegir el autobús

#######
#(c)
#######
#Rendimiento de la regla de clasificación:

#Estimación optimista (aparente o empírica)
predi<- predict(anadis)
str(predi)
#Contiene 3 componentes
#class: clasificación con la regla de Fisher
#posterior: estimación de las probabilidades de
#           automóvil y autobús
#x: FLD
#"matriz de confusión"
confu<-table(Real=datos$tipo,Predic=predi$class)
confu
100*prop.table(confu,1)
100*diag(prop.table(confu,1)) #Acierto en cada clase
100*sum(diag(confu))/sum(confu) #Acierto global

#Estimación mediante Jackknife 
anadisJ<-lda(datos[,-4],datos[,4],CV=TRUE)
str(anadisJ)
#Almacenar la clasificación y las estimaciones de
#probabilidades auto/bus para cada caso utilizando
#el modelo ajustado sobre el resto de casos
prediJ<- anadisJ$class
#matriz de confusión
confuJ<-table(Real=datos$tipo,PredJ=prediJ)
confuJ
100*prop.table(confuJ,1)
100*diag(prop.table(confuJ,1)) #Acierto en cada clase
100*sum(diag(confuJ))/sum(confuJ) #Acierto global

# CONSTRUCCIÓN DE LA CURVA ROC ASOCIADA A LA REGLA DISCRIMINANTE
library(ROCR)  

# La regla creada clasifica en "BUS" si la probabilidad a posteriori es mayor que 0.5
# Se puede comprobar:
probabiJ<- anadisJ$posterior[,2]  # Vector de probabilidades(J) posteriores  de "bus"
by(probabiJ,prediJ,summary)
#o bien
boxplot(probabiJ~prediJ)
abline(h=0.5,col="blue",lty=2)
# Se pretende crear la curva ROC para determinar el uso del "BUS" a través de las
# probabilidades a posteriori determinadas como resultado del análisis discriminante.
# 
# La curva ROC representa 1a especificidad (ES) frente a "1 menos la sensibilidad" (1-SE) para
# cada posible valor umbral o punto de corte en la escala de resultados de la prueba.
#
# Especificidad = probabilidad de, dado un sujeto que NO use BUS, que la regla
#                 lo clasifique como "NO usa BUS" (ratio de "Verdaderos Negativos")
# Sensibilidad  = probabilidad de, dado un sujeto que use BUS, que la regla
#                 lo clasifique como "usa BUS" (ratio de "Verdaderos Positivos")
# Curva Roc     = Eje X : 1-Especificidad= ratio de Falsos Positivos )
#                 Eje Y : Sensibilidad = ratio Verdaderos Positivos


# Considerando la distribuciones condicionadas a lo observado(filas)
prop.table(confuJ,1)
# La especificidad considerando este punto de corte (0.5) es: 0.8224044
# Punto en el eje X =  0.1775956
# La sensibilidad considerando este punto de corte (0.5) es: 0.8442623
# Punto en la curva ROC sería: (0.1775956   , 0.8442623 )


prediobj<-prediction(probabiJ,datos[,4])  # Creación del objeto estandarizado para la curva ROC

plot(performance(prediobj, "tpr","fpr"),
     main="CURVA COR (Jackknife)")
abline(a=0,b=1,col="blue",lty=2)
points(0.1775956,0.8442623,col=2,cex=1.5,pch=15) # Punto asociado prob.posterior=0.5

# Medida de la bondad del criterio: área bajo la curva ROC (AUC)
# Cuanto más cercano a 1, mejor predictor.

AreaBajocurva<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",AreaBajocurva,"\n")

#######
#(d)
#######
#Análisis discriminante cuadrático

modeloq<-qda(tipo~.,datos,CV=TRUE)
prediqJ<- modeloq$class

#matriz de confusión
confuQJ<-table(Real=datos[,4],PredQJ=prediqJ)
confuQJ
100*diag(prop.table(confuQJ,1)) #Acierto en cada clase
100*sum(diag(confuQJ))/sum(confuQJ) #Acierto global


library(ROCR)
probabi<- modeloq$posterior[,2]
prediobj<-prediction(probabi,datos[,4])
auc<- as.numeric(performance(prediobj,"auc")@y.values)
cat("AUC test= ",auc,"\n")
plot(performance(prediobj, "tpr","fpr"),main="CURVA COR TEST")
abline(a=0,b=1,col="blue",lty=2)

cat("AUC test DISCRIMINANTE CUADRÁTICO= ",auc,"\n")
cat("AUC test DISCRIMINANTE LINEAL= ",AreaBajocurva,"\n")

########################
#3. Datos iris. 3 clases
########################
#######
# (a)
######
library(MASS)
?iris
data(iris)
head(iris)
tail(iris)
summary(iris)
by(iris[,1:4],iris[,5],summary)
by(iris[,1:4],iris[,5],colMeans)
by(iris[,1:4],iris[,5],Test_Mardia)
MBox(iris[,1:4],iris[,5]) #se rechaza

iris.color<-c("red","green","blue")[iris$Species]
pairs(iris[,-5],col=iris.color)


#######
# (b)
######

#Redefinir niveles a una sola letra
#Útil para plot.lda
levels(iris$Species)<- c("s","v","g") 
iris.lda<- lda(iris[,-5],iris[,5])
iris.lda 

#Gráficos
plot(iris.lda,col=iris.color)
abline(h=0,v=0,lty=2)
grid()
centrosLD<-predict(iris.lda,iris.lda$means)$x
text(centrosLD,labels=rownames(centrosLD),
     cex=2,col=c("red","green","blue"))

#LD1: separa razonablemente los grupos
#LD2: poca capacidad de separación

#Estimación del acierto con Jackknife
iris.ldaCV<- lda(iris[,-5],iris[,5],CV=T)
confuJ=table(Real=iris[,5],PredJ=iris.ldaCV$class)
100*prop.table(confuJ,1)
100*diag(prop.table(confuJ,1)) #Acierto en cada clase
100*sum(diag(confuJ))/sum(confuJ) #Acierto global

##################
#4. Un sistema OCR
##################

#######
# (a)
#######

datos<-read.table("digitosOCR.txt",header=TRUE)
dim(datos)
names(datos)
table(datos[,1])
head(datos)
summary(datos)  
#Cada pixel es una intensidad de gris en [-1,1]
#Para pasar de [-1,1] a [0,1];
datos[,2:257]=((-1)*datos[,2:257]+1)/2  
#ya que pixmap para una imagen en escala de grises, 
#considera valores en [0,1], 0= negro, 1=blanco

#######
# (b)
#######
library(pixmap)
parantig<-par(mfrow=c(5,5),
              mar=c(0,0,0,0)+1,bg="lightblue")
for (i in 1:25)
{matriz<- datos[i,2:257]
 x<-pixmapGrey(matriz,16,16,cellres=1)
 x@grey<-t(x@grey)
 plot(x)
}
par(parantig)  #Restablece las opciones

#######
# (c)
#######
set.seed(12345)
n=nrow(datos)
ient=sample(n,floor(n*0.75))  #floor(x) entero inferior de x más cercano
length(ient)
datosent=datos[ient,] #Conjunto de entrenamiento
datostest=datos[-ient,] #Conjunto test

#######
# (d)
#######

#Reducir la dimensionalidad mediante ACP
acp<- princomp(datosent[,-1],cor=TRUE)
summary(acp)
varcp<- acp$sdev^2
porcen<-100*varcp/sum(varcp)
cbind(1:50,varcp[1:50],porcen[1:50],
      acum=cumsum(porcen)[1:50])
m<- 45  # componentes principales con var >1 (83.88% de la varianza total)

# Aplicación del Análisis Discriminante sobre las CP seleccionadas
puntucp<- acp$scores[,1:m]
dim(puntucp)

#ADL Fisher
library(MASS)
modelo.lda<- lda(puntucp,datosent[,1])
modelo.lda
plot(modelo.lda,dimen=2,cex=0.6,
     main="ADL sobre las puntuaciones ACP")
abline(v=0,h=0,lty=2)
grid()

#######
# (e)
#######
# Puntuaciones de las CP sobre el conjunto test
puntucptest=predict(acp,datostest)[,1:m]
dim(puntucptest)

# Predicciones del lda sobre el conjunto test
preditest=predict(modelo.lda,puntucptest)$class

# Estimación de las tasas de acierto del modelo en el conjunto test.
confutest=table(Real=datos[-ient,1],Predtest=preditest)
confutest

#Podemos obtener los aciertos como en los
#ejemplos anteriores
100*diag(prop.table(confutest,1)) #Acierto en cada clase
100*sum(diag(confutest))/sum(confutest) #Acierto global

#Esto no funcionaría si algún dígito no aparece
#en las predicciones
#Directamente:

tacier<- 100*mean(datostest[,1]==preditest)
cat("Tasa de acierto (test)= ",tacier ,"% \n")
tac=numeric(10)
names(tac)=0:9
for (i in 1:10)
{
  selec_i=which(datostest[,1]==i-1)
  tac[i]=100*mean(datostest[selec_i,1]==preditest[selec_i])
}
cbind(tac)

#Visualizarlo como una columna adicional
#a la tabla confutest
cbind(confutest,round(tac,2))
barplot(tac,main="% Acierto test")
abline(h=tacier,col="blue",lwd=3)

#######
# (f)
#######

# Selección de casos erróneos
casos_err=which(datostest[,1]!=preditest)
length(casos_err)

sum(confutest)-sum(diag(confutest)) #Comprobación
casos_err

# Selección aleatoria de 25
sel25=sample(casos_err,25)
parantig<-par(mfrow=c(5,5),
              mar=c(0,0,0,0)+1,bg="lightblue")
for (i in sel25)
{matriz<- datostest[i,2:257]
x<-pixmapGrey(matriz,16,16,cellres=1)
x@grey<-t(x@grey)
plot(x,main=paste("R=",datostest[i,1],"Pr=",preditest[i]),cex.main=1.3)
}
par(parantig)  #Restablece las opciones

