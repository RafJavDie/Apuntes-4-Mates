##############################################
#FACULTAD DE MATEMÁTICAS                     #
#UNIVERSIDAD DE SEVILLA                      #
#GRADO EN MATEMÁTICAS                        #
#DOBLE GRADO EN MATEMÁTICAS Y ESTADÍSTICA    #
#ANÁLISIS DE DATOS MULTIVARIANTES            #
#HOJA 1 DE PROBLEMAS                         #
#EJERCICIOS CON VECTORES Y MATRICES          #
##############################################


#1.Crear un vector donde se repitan los códigos 
# provinciales de Andalucía
# 10 veces, 15 para Málaga y 18 Sevilla

codigos=c(4,11,14,18,21,23,29,41)
#acceder a elementos:
codigos[3]; codigos[c(1,4)]; codigos[2:4]
codigoprov=rep(codigos,c(rep(10,6),15,18))
codigoprov
codigoprov=sample(codigoprov)
table(codigoprov)

#2. Con la ayuda de paste, crear un vector de nombres
#"Caso_1",...,"Caso_30"

paste("Caso",1:30,sep="_")
#paste("Caso ",1:30,sep="")

#3. Generar dos vectores de tamaño 250, 
#   seleccionando aleatoriamente
#   números enteros entre 0 y 999
set.seed(1357) #semilla del generador
n=250
x=sample(0:999,n,rep=T)
y=sample(0:999,n,rep=T)

#i)Visualizarlos en dos columnas
cbind(x,y)

#Ver las primeras filas o las últimas
head(cbind(x,y))
tail(cbind(x,y))

#ii)Construir el vector y2-x1,...,y250-x249
dife=y[-1]-x[-n]
#o bien y[2:n]-x[1:(n-1)]
#Comprobación:
xy=cbind(x,y)
cbind(xy,c(NA,dife))[1:10,]

#iii)Construir el vector y2-y1,...,y250-y249
y[-1]-y[-n]
diff(y)

#iv) Construir el vector x1+2x2-x3,x2+2x3-x4,...,x248+2x249-x250
x[1:(n-2)]+2*x[2:(n-1)]-x[3:n]

#v)Calcular la suma de los valores 1/(xi+yi)
sum(1/(x+y))


#4. Siguiendo con x e y
#i) Determinar las posiciones y valores de los yi>600
which(y>600)
y[y>600]


#ii)Construir una matriz con las posiciones y valores 
#anteriores, y con los 
#valores de x en esas posiciones
posic_y=which(y>600)
matriz=cbind(posic_y,y[posic_y],x[posic_y])
colnames(matriz)=c("Posiciones","y","x")
matriz

#iii) Guardar las posiciones como nombres de filas
rownames(matriz)=matriz[,1]
matriz=matriz[,-1]
matriz
rownames(matriz)
#Convertir cadenas a tipo numérico
as.numeric(rownames(matriz))

#iv)Construir el vector |x1-xmedia|^(1/2),....,|xn-xmedia|^(1/2)
xmedia=mean(x)
difxabs=sqrt(abs(x-xmedia))
plot(x,type="h")
abline(h=xmedia,col="blue",lwd=2)

plot(difxabs,type="h")

#v) Calcular el número de elementos de y que distan menos de 200 del
#máximo de y

sum( max(y)-y <200)
which(max(y)-y <200)

#vi) ¿Cuántos elementos de x son pares?
sum(x%%2 ==0)

#vii) Seleccionar los elementos de x en posiciones donde 
#y sea múltiplo de 5
x[y%%5 ==0]

#viii) Ordenar los elementos de x según la ordenación creciente de y
sort(y)   #ordenar los elementos de y
order(y)  #rango de cada elemento de y 
y[order(y)] #lo mismo que sort
x[order(y)]
cbind(x[order(y)],y[order(y)])


#5. Calcular 1+ (1+2) + ...+ (1+2+3..+10)
cumsum(1:10)
sum(cumsum(1:10))

#6. Calcular 1+ (2/3) + (2/3)(4/5)+...+(2/3)(4/5)(6/7)+...+ 
#( (2/3)(4/5)(6/7)...(38/39) )
cbind(seq(2,38,2),seq(3,39,2)) #visualizar ambas secuencias 
x=c(1,seq(2,38,2)/seq(3,39,2))
cumprod(x)
sum(cumprod(x))

#7. Construir una matriz n x n con 0 en la diagonal, 
#+1 en la mitad triangular superior
#y -1 en la mitad triangular inferior
#Se puede hacer de diversas formas
n=5
A=matrix(0,n,n)

A[upper.tri(A)]=1
A

A[lower.tri(A)]=-1
A

#acceso a elementos
#A[1,3]; A[-1,]; A[,3]; A[c(1,4),3]
#diag(A)

#8. Construir una matriz con la tabla de multiplicar
n=9
m=9
i=1:n
j=1:m
A=outer(i,j,"9")
colnames(A)=paste("*",1:m,sep="")
rownames(A)=1:n
A

#otra opción (menos eficiente):
A=matrix(NA,n,m)
for (i in 1:n)
  for (j in 1:m)
    A[i,j]=i*j
A

#también
A=cbind(1:n)%*%rbind(1:m)

#9. Construir una matriz 6x9 con enteros aleatorios en 1,..10
set.seed(12345)
n=6
m=9

A=matrix(sample(10,size=n*m,rep=TRUE),n,m)

#i)Calcular la suma de cada fila, visualizarlo en 
#una nueva columna
apply(A,1, sum)
cbind(A,Total=apply(A,1, sum))


#ii) Calcular el máximo de cada columna, visualizarlo 
#en una fila nueva
apply(A,2,max)
rbind(A,Maximo=apply(A,2,max))

#iii) Calcular A*At
A
t(A)
A%*%t(A)
