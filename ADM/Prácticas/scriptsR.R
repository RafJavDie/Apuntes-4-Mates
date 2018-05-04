################################
#    Scripts Útiles para R     #
################################
#                              #
################################

################################
#         Introducción         #
################################

# Leer datos de entrada
datos<- read.table("datos.extension",header=TRUE,row.names=1)
str(datos)      # ESTRUCTURA DEL OBJETO
datos$v1        # ACCEDER A UNA VARIABLE
dim(datos)      # NUMERO DE FILAS Y COLUMNAS
summary(datos)  # RESUMEN NUMERICO
attach(datos)   # PARA USAR DIRECTAMENTE EL NOMBRE DE LAS VARIABLES

# Plot simple de 2 variables
# Si DATOS tiene más, coge las dos primeras

plot(v1,v2,main="Título del gráfico", xlim=c(110,180)) #PUNTOS
text(v1,v2,label=rownames(datos),adj=1,col="red")
grid()          # REJILLA

# Recta de regresión 
regre<-lm(Peso~Altura)          # V.DEPEND.~V.INDEPEND.
abline(regre,lwd=2,col="blue")  # Plotea una recta en la gráfica anterior

# La función pairs da los plots de las correlaciones

# Boxplot
boxplot(v1)  # Clásico diagrama de bigotes y cajas
## Representa bloxplots de v1 spliteados según v2
boxplot(v1~v2,main="Título",xlab="v2",names=c("vector nombres variable x")) 

# Apply es ideal para trabajar con matrices
apply(decath,1,var)
apply(decath,2,sd)

# Dibujar Medias 
matplot(matriz,type="l",xlab="Eje x",ylab="Eje Y",lwd=2,
        lty=1:ng, main="Título",col=1:ng)
## Con el comando lines podemos dibujar sobre la gráfica anterior la línea de las medias.

##############################
##############################
#         Contraste          #
#           Normal           #
#        Univariante         #
##############################
##############################

# La función ananor(x) [hay que importarla] mide la normalidad univariante
# de una muestra de aleatoria, al igual que el Test de Shapiro

# La función t.test(x,mu=a) testea la hipótesis H0: E[X]=a. Dejamos los comandos
# para ilustrar lo observado

resul<-  t.test(x,mu=a);

## Dentro de resul tenemos opciones como $parameter, etc.

# Si tenemos dos poblaciones y queremos testear H0:E[x]=E[y], testeamos la
# igualdad de varianzas primero con

var.test(x,y) 

# Luego volvemos a realziar el contraste. Con "alt" podemos dar el caracter
# de la hipótesis alternativa. Si H1:E[x]<E[y] entonces

t.test(x,y,alt="less",var.equal=TRUE) # Por defecto False


##############################
##############################
#         Contraste          #
#           Normal           #
#        Multivariante       #
##############################
##############################

# Estudio de la normalidad multivariante. 

# Muchas veces necesitaremos un FACTOR que identifique los grupos dentro
# de un conjunto de datos. Tenemos dos opciones, tomarlos separados de los datos
g = factor("Aquí tienes que meter un vector")

# O bien hacer que una columna sea un factor [preguntar]
datos$v3<- factor(datos$v3)
levels(datos$v3)<- c("Si quieres puedes ponerle nombres en vez de numeros")

## Plots de datos donde g es un FACTOR de los grupos. ¿Qué son los niveles? [PREGUNTAR]
g = factor("Aquí tienes que meter un vector")
plot(Z,col=c("red","blue")[g])
pairs(Z,col=c("red","blue")[g])

## Para aplicar funciones por grupos
by(Z, g,funcion)

### Por ejemplo, la media
medias=by(Z,g,function(M)  apply(M,2,mean));
media<- apply(datos,2,mean)  # Otra opción
## Manera más obvia de calcular la media y plotearla sobre 

## Test de Shapiro
apply(datos,2,shapiro.test)

## Test de Mardia, Este es mejor porque se aplica 
source("Test_Mardia.R")
mardiaTest(datos)
by(datos,g,mardiaTest)

# Contraste H0:E[x]=vector.
HotellingsT2(datos,mu = c(0,0,0))   

# Si tenemos dos poblaciones podemos usar el comando anterior de dos maneras 
# distintas en función de si tenemos o no separados los datos (PREGUNTAR). para
# el contraste H0: E[x]-E[y]=mu

## Si tenemos dos matrices de datos (no necesariamente independientes) [PREGUNTAR]
HotellingsT2(X,Y, mu = rep(-0.5,4))

## Si lo tenemos separados con un factor podemos meter directamente la fórmula
## Preguntar
HotellingsT2(Z ~ g, mu = rep(-0.5,4))

# Paralelimos
HotellingsT2(AZ ~ g)

# Perfil medio
Hotelling(1sZ ~g)

##############################
##############################
#          Análisis          #
#             de             #
#       Conglomerados        #
##############################

# Comencemos por lo básico. Método jerárquico aglomerativo. Hclust con método complete.
# Puedes usar otros métodos como "single" o "average". 
D<- dist(datos)
round(D,1)
conglo<- hclust(D,method="complete") ## Solo acepta la matriz de disimilaridad
plot(conglo, cex=0.8,labels=rownames(datos),
     main="Dendrograma",xlab="Completo",
     sub="Distancias",ylab="Distancias",col="blue")

## preguntar 
cbind(conglo$merge,conglo$height) 

# Si queremos solo una cantidad específica de clusters (ej k=3), computamos
cluster <- cutree(conglo, k = 3)
table(cluster)
split(mamiferos,cluster)
## Lo mismo con otro formato
by(mamiferos,cluster,function(x) x) 

## Comparar cutrees con table(tabla1,tabla2)

# Calcular los centroides:
Centros=by(mamiferos,cluster,colMeans)                #(Formato list) 
Centros=t(sapply(split(mamiferos,cluster),colMeans))  #(Formato matrix)

#ESTADISTICO F DEL ANOVA de 1 FACTOR. Así vemos las variables con mayor variabilidad.

apply(mamiferos,2, function(x) 
  summary(lm(x~factor(cluster)))$fstatistic[1])

## TIPICO PLOT DE VARIABLES (EJ CON SWISS)
plot(swiss[,c(5,2)],type="n",
     main="Swiss, KM")
text(swiss[,c(5,2)],labels=row.names(swiss),
     cex=0.6,col=colores[km$cluster])
text(km$centers[,5],km$centers[,2],
     labels=row.names(Centros),cex=1.5,col=colores)


##### ¿QUÉ COÑO ES AGNES? ##### ¿Con agnes se calcula además el coeficiente de
##### aglomeración? Puedes darle como entrada tanto la matriz de observaciones
##### como la matriz de disimilaridad, dependiendo del valor de diss. Creo que por
##### defecto es falsote
agnclus <- agnes(mamiferos, metric = "euclidean", # Ejemplos de agnes con datos
                 stand = FALSE,method="complete") # y método complete

# Si ploteas el agnes salen dos plots: banner y dendograma

### Diferencias entre plot de agnes de barras y el normal.

agnclusin$ac  #Coeficiente de aglomeracion


#### ¿Qué cojones hace daisy? ¿Calcula el coeficiente de bower? Ceo que sí, pero que 
#### habría que especificarle en la metric.

D <- daisy(flower,type = list(asymm = c("V1", "V3"), symm= 2,ordratio= 7,logratio= 8))
D





#### Pasamos al método de partición. En este caso k-medias. Tenemos que partir de k
#### centros iniciales. Por ejemplo, podemos partir de los que nos da un análisis jerárquico
#### La entrada PARECEN tener que ser la matriz de DATOS. Si damos los centros iniciales
#### entonces los damos con formato matrix
data(swiss)
h<- hclust(dist(swiss),method="complete")
plot(h)
cluster<- cutree(h,3)
Centros= t(sapply(split(swiss,cluster),colMeans))  #Formato matrix
km<- kmeans(swiss,Centros)   
km
table(km$cluster)

### Sin embargo también podemos utilizar kmeans dando los centros que queremos y con 
### un cierto numero de soluciones iniciales. En el ejemplo ponemos 15
km3 <- kmeans(swiss, 3, nstart = 15)


### Calcular Error Cuadratico Medio
ECM<- mean((datos-datoscom)^2)

### Preguntar por datoscom<- km$centers[km$cluster,] (5.4.ii) en general. Segunda vez que
### lo veo y suscribo nuevamente la pregunta crack.




####### Método de K-medioides. Funcion pam.`Al igual que kmeans acepta tanto datamatrix 
####### como matriz de disimilaridad.` 
agric.pam<- pam(agriculture,2) 
## El plot nos da dos gráficas por las que habría que preguntar la verdad. 
plot(agric.pam)
## Recuerda que en help puedes ver qUe $id.med son los índices de los medioides en la matriz
## mientras que $mediodes son las posiciones de los mediodes.

## Lo buena o mala que es una solución podemos verlo con agric.pam.i$silinfo$avg.width.
## Cuanto más cercano a 1  mejor es la solución. Menos de 0.50 mal asuntillo.
## Por ejemplo, usando tenemos una gráfica muy bonita de la calidad en función del numero
## de medioides

plot(2:10,sapply(2:10,function(x) pam(ruspini,x)$silinfo$avg.width ),type="l")

