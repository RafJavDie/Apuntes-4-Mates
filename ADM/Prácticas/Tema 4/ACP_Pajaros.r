################################################
#ANALISIS DE DATOS MULTIVARIANTES.             #  
#GRADO EN MATEMÁTICAS.                         #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA      #
#ANALISIS DE COMPONENTES PRINCIPALES           #
#CON UNA MATRIZ DE CORRELACIONES               #
#MEDIDAS DE HUESOS DE AVES                     #
################################################

#####################################
#1. Definir R y test de Barlett
#####################################
R=matrix(c(1,0.584,0.615,0.601,0.570,0.600,
            0.584,1,0.576,0.530,0.526,0.555,
            0.615,0.576,1,0.940,0.875,0.878,
            0.601,0.530,0.940,1,0.877,0.886,
            0.570,0.526,0.875,0.877,1,0.924,
            0.600,0.555,0.878,0.886,0.924,1),6,6)
colnames(R)= rownames(R)= c("longcra","anchcra",
                              "humero","cubito","femur","tibia")
R
R-t(R)  #Comprobar que es simétrica


# El test de esfericidad de Bartlett contrasta 
# la hipótesis nula de que una matriz
# var-cov es proporcional a la identidad, 
# si se trabaja con las variables tipificadas
# H0:R=I ,  H1:R!=I

esfbarl <- function(M){
  n=400
  p= ncol(M)
  EBarl = -(n-1-(2*p+5)/6)*log(det(M))
  gl = p*(p-1)/2
  EBarl
  gl
  s = pchisq(EBarl,gl,lower.tail=FALSE)
  return(s)
}
esfbarl(R)
#Significativo  ## ¿Cuando rechazamos?
                                                    

#########################################
#2. Análisis de componentes principales
#########################################
autoval= eigen(R)$values
autovec= eigen(R)$vectors
#Gráfico de sedimentación
plot(autoval,type="h",main="Datos de los pájaros",ylab="Autovalor",
xlab="Componente",col="red")
abline(h=mean(autoval),lwd=3,lty=3,col="blue")

#Una sola C.P. explica el 76%, mejor dos C.P.
resumen= matrix(NA,nrow=length(autoval),ncol=3)
resumen[,1]=  autoval
resumen[,2]= 100*resumen[,1]/sum(resumen[,1])
resumen[,3]= cumsum(resumen[,2])
colnames(resumen)= c("Autovalor","Porcentaje","Porcentaje acumulado")
resumen
m=2  #Número de componentes a considerar           
#Matriz de coeficientes que definen cada 
#combinación lineal:
L= autovec[,1:m] 
rownames(L)=colnames(R)
#para calcular las correlaciones entre las variables y las componentes
cor_vc=L%*%diag(sqrt(autoval[1:m]))               
cor_vc                                            

#Comunalidades: para cada variable, es 
#la suma de correlaciones cuadrado
#con las c.p. seleccionadas
cbind(apply(cor_vc^2,1,sum))       


#########################################
#3. Análisis de componentes principales
#########################################

#La rotación ortogonal varimax
#facilita la interpretación
(rotvmax= varimax(L))
Lrot=rotvmax$loadings
rownames(Lrot)=colnames(R)
Lrot #Más fácil de interpretar

plot(L,type="n",xlim=c(-1,1),ylim=c(-1,1),
     main="", xlab="CP 1",ylab="CP 2")  
text(L,labels=rownames(L),cex=0.8)
text(Lrot,labels=rownames(Lrot),cex=0.8,col="red")
grid()
abline(h=0,v=0,lty=2)

