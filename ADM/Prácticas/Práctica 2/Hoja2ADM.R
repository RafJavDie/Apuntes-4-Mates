##############################################
#FACULTAD DE MATEMÁTICAS                     #
#UNIVERSIDAD DE SEVILLA                      #
#GRADO EN MATEMÁTICAS                        #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA    #
#ANÁLISIS DE DATOS MULTIVARIANTES            #
#HOJA 2 DE PROBLEMAS                         #
##############################################

#1. Datos Familia.txt
######################
#i)
#LEER UN FICHERO DE TEXTO, TIENE UNA CABECERA 
#CON LOS NOMBRES DE LAS VARIABLES
#LA PRIMERA COLUMNA DEL FICHERO SE VA A USAR 
#COMO ETIQUETA IDENTIFICATIVA
datos<- read.table("Familia.txt",header=TRUE,row.names=1)
datos
str(datos)      #ESTRUCTURA DEL OBJETO
datos$Altura    #ACCEDER A UNA VARIABLE
datos[,1]       #ACCEDER A FILAS Y/O COLUMNAS
datos[c(2:3),]
dim(datos)      #NUMERO DE FILAS Y COLUMNAS
summary(datos)  #RESUMEN NUMERICO
apply(datos,2,mean)  #mean(datos[,1]) ; mean(datos[,2])
apply(datos,2,sd)    #CUASIDESV. TIPICA DE CADA COLUMNA


#ii) NUBE DE PUNTOS
attach(datos)    #PARA USAR DIRECTAMENTE EL NOMBRE DE LAS VARIABLES
plot(Altura,Peso,main="Datos familiares", xlim=c(110,180)) #PUNTOS
text(Altura,Peso,label=rownames(datos),adj=1,col="red")   
#TEXTOS; adj controla la posición del texto respecto al punto
grid()      #REJILLA

regre<-lm(Peso~Altura)          #V.DEPEND.~V.INDEPEND.
abline(regre,lwd=2,col="blue")
summary(regre)
str(regre)                      #ESTRUCTURA DEL OBJETO regre
regre$coefficients              #coef(regre)
help(lm)                        #AYUDA SOBRE LA FUNCION lm
cor(Altura,Peso)                #COEFICIENTE CORR. LINEAL

#iii) PESO
boxplot(Peso,main="Peso",col="lightblue")  #CAJA Y BIGOTES
#ver help(boxplot) y help(boxplot.stats) para 
#detalles sobre el cálculo de los elementos de 
#esta representación gráfica
str(boxplot(Peso))   
datos[datos$Peso==boxplot(Peso)$out,]

#ALTURA
boxplot(Altura,main="Altura",col="lightgrey") #CAJA Y BIGOTES
datos[datos$Peso==boxplot(Peso)$out,]

#iv). REPETIR EL AJUSTE DEL MODELO REG. LINEAL SIN EL VALOR OUTLIER
posic_Sob<- which(rownames(datos)=="Sobrina")   #POSICION SOBRINA
plot(datos[-posic_Sob,],main="Datos familiares (sin la sobrina)",
     xlim=c(110,180))
text(datos[-posic_Sob,],label=rownames(datos)[-posic_Sob],adj=2,col="blue")
grid()

#SE REDUCE LA CORR. LINEAL
regre2<-lm(Peso~Altura, data=datos[-posic_Sob,])    
abline(regre2,lwd=2,col="blue")
summary(regre2)
cor(datos[-posic_Sob,])

#PARA VER MEJOR LA REDUCCIÓN DE LA INTENSIDAD DE LA
#RELACIÓN LINEAL:
plot(Altura,Peso,main="Datos familiares",xlim=c(110,180)) 
text(Altura,Peso,label=rownames(datos),adj=1)   
grid()                                                    
abline(regre,lwd=2,col="blue")
abline(regre2,lwd=2,col="red")

#v). IMC
IMC<- Peso/((Altura/100)^2)
names(IMC)=rownames(datos)
barplot(IMC,cex.names=0.7,main="IMC Familiar")
abline(h=0)

#v) Ordenar
datosIMC<- data.frame(datos,IMC)
datosIMC[order(-datosIMC$IMC),]

selec<- subset(datosIMC,(IMC<25) & (IMC>20))
selec[order(-selec$IMC),] #DE MAYOR A MENOR



#2. DATOS Boston
################

#i) ACCESO A DATOS YA DISPONIBLES EN R
library(MASS)
data(Boston)
?Boston
str(Boston)
dim(Boston)
summary(Boston)

##ii)
pairs(Boston)
R<-cor(Boston)
round(R,2)
Rabs<- abs(R)
round(Rabs,2)
sort(Rabs[14,-14],decreasing=TRUE)
names(sort(Rabs[14,-14],decreasing=TRUE))

##iii)
attach(Boston)
boxplot(crim)
which(crim>60)
boxplot(tax)
boxplot(ptratio)
boxplot(ptratio)$out
which(ptratio<=13)

##iv)
table(chas)
boxplot(crim~chas,main="Criminalidad",xlab="VECINDAD AL RIO",names=c("NO","SI"))
boxplot(medv~chas,main="Valor de las viviendas",
        xlab="VECINDAD AL RIO",names=c("NO","SI"))
sapply(split(crim,chas),mean)
sapply(split(medv,chas),mean)

#ALTERNATIVA SIN sapply
mean(crim[chas==0])
mean(crim[chas==1])

##v)
plot(lstat,medv)
reg1<- lm(medv~lstat)
summary(reg1)
abline(reg1,col="red",lwd=2)

plot(lstat,log(medv))
reg2<- lm(log(medv)~lstat)
summary(reg2)
abline(reg2,col="red",lwd=2)

##3. DATOS CONTAMINACION ATMOSFERICA
####################################
#i) ACCEDER A LOS DATOS
install.packages("HSAUR2")  #INSTALAR UN PAQUETE
library(HSAUR2)
data("USairpollution")
?USairpollution
summary(USairpollution)
pairs(USairpollution)
R<-cor(USairpollution)
round(R,2)

#ii) NUBE DE PUNTOS
plot(popul~manu,data=USairpollution,
     xlab="Emp. Fabric. con al menos 20 trabajadores",
     ylab="POBLACION (miles)")
grid()
#IDENTIFICAR PUNTOS USANDO LOS NOMBRES DE LAS CIUDADES
identify(USairpollution$manu,USairpollution$popul,
         rownames(USairpollution))
rug(USairpollution$manu,side=1)
rug(USairpollution$popul,side=2)

#iii) NUBE DE PUNTOS Y BOXPLOT E HISTOGRAMA
#Para Rstudio, puede ser necesario ejecutar antes
#las siguientes líneas y la última 
#graphics.off() 
#antig.par = par(no.readonly = TRUE) #guardar parám. gráf.
#par("mar") #Márgenes (líneas) alrededor de cada gráfico
#par(mar=c(1,1,1,1)) #Los reducimos
#Con layout se define la ubicación de cada uno de los siguientes
#gráficos tras dividir en 4 partes la consola de gráficos
#[2 0] Gráfico 1: posición inferior izquierda
#[1 3] Gráfico 2: posición superior izquierda
#      Gráfico 3: posición inferior derecha
#      Sin uso: posición superior derecha
layout(matrix(c(2,0,1,3),nrow=2,byrow=TRUE),widths=c(2,1),
       heights=c(1,2), respect=TRUE)
xlim=range(USairpollution$popul*1.1)
plot(popul~manu,data=USairpollution,
    xlab="Emp. Fabric. con al menos 20 trabajadores",
    ylab="POBLACION (miles)",type="n",cex.lab=0.9)
with(USairpollution,text(manu,popul,cex=0.6,
     labels=abbreviate(rownames(USairpollution))))
with(USairpollution,hist(manu,main="",xlim=xlim))
with(USairpollution,boxplot(popul))
#par(antig.par)  


##4.DATOS DECATHLON
##################

#i) LECTURA DE LOS DATOS
########################

library(foreign)
decath<- data.frame(read.spss("decathlon1989.sav"))

nombres<- decath$NOMPAIS
#ELIMINAR LA VARIABLE NOMBRES, SE ALMACENA COMO ETIQUETAS:
decath<- decath[,1:10]    
rownames(decath)<- nombres
decath


#Resumen numérico:
summary(decath)
colMeans(decath) #apply(decath,2,mean)
apply(decath,2,var)
apply(decath,2,sd)
var(decath)
round(var(decath),2)  #también cov
diag(var(decath))
round(cor(decath),2)

#ii) BOXPLOTS
##############

boxplot(decath)
par(mfrow=c(2,5))
for (i in 1:ncol(decath))
{ boxplot(decath[,i],main=names(decath)[i]) }
par(mfrow=c(1,1))


#iii) NUBES DE PUNTOS
###################
#HISTOGRAMAS EN LA DIAGONAL DE PAIRS
panel.hist <- function(x)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
}

pairs(decath,diag.panel=panel.hist,main="Decathlon 1989")

#SUPERPONER LA RECTA DE MINIMOS CUADRADOS A CADA NUBE DE PUNTOS
panel.rectareg<-function (x, y, bg = NA, pch = par("pch"), 
                          color.punto="blue",color.recta = "red", 
                          lwd.recta=2) 
{
  points(x, y, pch = pch, col = color.punto, bg = bg)
  recta<- lsfit(x,y)
  abline(recta,col=color.recta,lwd=lwd.recta)
}
pairs(decath,panel=panel.rectareg,diag.panel=panel.hist,
      main="Decathlon 1989")

#DIBUJAR SUAVIZACION DE LAS NUBES DE PUNTOS
panel.smooth<-function (x, y, bg = NA, pch = par("pch"),lwd.smooth=2, 
                        cex = 1, color.punto="black",col.smooth = "red", 
                        span = 2/3, iter = 3) 
{
  points(x, y, pch = pch, col = color.punto, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), lwd=lwd.smooth,
          col = col.smooth)
}
pairs(decath,panel=panel.smooth,diag.panel=panel.hist,main="Decathlon 1989")

#MITAD SUPERIOR: VALOR ABSOLUTO DEL COEFICIENTE DE CORRELACION LINEAL
panel.cor<- function(x,y,digits=2,prefix="",cex.cor)
{
  usr<-par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r<- abs(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if (missing(cex.cor)) cex<- 0.8/strwidth(txt)
  text(0.5,0.5,txt,cex=cex*r)
}
pairs(decath[,1:10],lower.panel=panel.smooth, upper.panel=panel.cor,
      diag.panel=panel.hist,main="Decathlon 1989")


#iv) USO DE corrplot
install.packages("corrplot")
library(corrplot)
corr<- cor(decath)
corrplot(corr, method="number", col="red")
corrplot(corr, method="number")
corrplot(corr)
corrplot(corr,method="square")
corrplot(corr,method="ellipse")
corrplot(corr, method="color")
corrplot(corr, method="shade")
corrplot(corr, method="pie")
corrplot(corr, type="upper", diag=FALSE)
corrplot(corr,type="lower")


#iv). RESUMEN NUMERICO
######################
summary(decath)
n<- nrow(decath)
p<- ncol(decath)
print(unos<- cbind(rep(1,n)))
print(En<- matrix(1,n,n))
print(I<- diag(n))

#MEDIAS
print(medias<- apply(decath,2,mean))
t(decath)%*%unos/n
str(t(decath)%*%unos/n)
str(medias)

#MATRIZ CENTRADA
xtilde<-(I-En/n)%*%as.matrix(decath)
summary(xtilde)
apply(xtilde,2,mean)

#MATRIZ DE DISPERSION
sigma<- var(decath)
sigma
S<-t(xtilde)%*%xtilde
S/(n-1) #coincide con sigma

eigen(sigma)$values
plot(eigen(sigma)$values,type="h")
cat("Varianza muestral generalizada=",det(sigma),"\n")
cat("Varianza muestral total=",sum(diag(sigma)),"\n")
cat("Varianza muestral efectiva=",det(sigma)^(1/p),"\n")

#TIPIFICACION (ESTANDARIZACIÓN)
z<- scale(decath)
attributes(z)
attr(z,"scaled:center")
attr(z,"scaled:scale")
summary(z)
var(z)
print(r<- cor(decath))  #COINCIDE CON LA MATRIZ ANTERIOR

#MATRICIALMENTE
dt<- apply(decath,2,sd)
sinv<-diag(1/dt)
Z<-xtilde%*%sinv
summary(z-xtilde%*%sinv)


#5. INFORME PISA 2009
#####################
#Cómo guardar objetos R en un espacio de trabajo
set.seed(12345)
x=sample(100,30,rep=TRUE)
y=sample(letters,30,rep=TRUE)
save(x,y,file="Espacio_trabajo.RData")
rm(list=ls()) #Borrar todo el espacio de trabajo actual
ls()  
load(file="Espacio_trabajo.RData")
ls()

#i) Cargar un espacio de trabajo
load(file="Pisa2009.RData") 
ls()
str(pisa)
head(pisa)
dim(pisa)
tail(pisa)
summary(pisa)

#ii) ORDENAR LOS PAISES

pisa[order(-pisa$Lectura),1:2]
pisa[order(-pisa$Matem),c(1,8)]
pisa[order(-pisa$Ciencias),c(1,9)]

#iii) Nubes de puntos, por ejemplo Lengua y Matemáticas
plot(pisa[,c(2,8)],type="n",xlab="Lengua",ylab="Matem",
     main="Informe PISA 2009")
text(pisa[,c(2,8)],col="blue",labels=pisa[,1],cex=0.6)
grid()

#Matriz de nubes de puntos
pairs(pisa[,-1])

#iv)
#Caras de Chernoff
install.packages("TeachingDemos")
library(TeachingDemos)
faces(pisa[1:36,-1],labels=pisa[1:36,1],main="Informe PISA 2009") 
#Estrellas
stars(pisa)
rownames(pisa)<- pisa[,1]

stars(pisa[,c(2,8,9)])
stars(pisa[,c(2,8,9)], key.loc = c(14, 2),
      main = "Informe PISA 2009", full = FALSE)
stars(pisa, len = 0.8, key.loc = c(12, 1.5), 
      main = "Informe PISA 2009", 
      draw.segments = TRUE)



#6. DIETAS ALIMENTICIAS
#######################
#i) 
datos<- read.table("dietas.dat")
str(datos) 
datos[,1]<- factor(datos[,1])
colnames(datos)<- c("Dieta",paste("Peso",1:6))
summary(datos)
#ii) CALCULAR LAS MEDIAS DE CADA DIETA
ng<-length(levels(datos$Dieta))
p<- ncol(datos)-1
medias<- matrix(NA,ng,p)
for (i in 1:ng)
  medias[i,]<- apply(datos[datos$Dieta==i,-1],2,mean)     
medias
rownames(medias)<- paste("Dieta ",1:ng,sep="")
medias
#LO MISMO EN UNA SOLA LINEA
t(sapply(split(datos[,-1],datos$Dieta),colMeans))

#DIBUJAR LAS MEDIAS
matplot(t(medias),type="l",xlab="Mes",ylab="Dif. con el peso ideal",lwd=2,
          lty=1:ng, main="EVOLUCION MEDIA DE LAS CUATRO DIETAS",col=1:ng)
mediaC<- apply(datos[,-1],2,mean)   #colMeans(datos[,-1])
lines(mediaC,col=5,lwd=4)
legend("topleft",lty=c(1:ng,1),lwd=2,legend=c(paste("Dieta ",1:ng),"Media"),
       col=1:5) 
grid()

#ii)
N=apply(datos[,-1],1, function(x) sum(x>3))
table(N)
which(N==5)

#7. ILUSTRACION DEL TEOREMA DE FISHER
######################################
#i)
M<-5000 
n<-10     
mu<-0
sigma<-1

muestras<- matrix(rnorm(M*n,mu,sigma),M,n)
medias<- apply(muestras,1,mean)    #rowMeans
cuasivar<- apply(muestras,1,var)

plot(medias,cuasivar,xlab="Media", ylab="Sc2",
     main="5000 muestras,n=10,N(0,1)",
col="red")
regre<-lm(cuasivar~medias)
summary(regre)
abline(regre,col="blue",lwd=2)
cor(medias,cuasivar)

#ii) AJUSTE chi^2_(n-1)
cocientes<- (n-1)*cuasivar/(sigma^2)   #suma de cuadrados/sigma^2
hist(cocientes,freq=FALSE,br=40, 
main="Ilustración del Teorema de Fisher\n 5000 muestras (n=10)  y f.d. Chi-9",
col="lightblue", ylab="Densidad y altura",xlab="(n-1)Sc2/sigma^2")
curve(dchisq(x,n-1),0,30,1000,add=TRUE,lwd=2)



