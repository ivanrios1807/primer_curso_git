Datos<-read.table("alfajores.txt",header = TRUE)
x_alfajoresdefectuosos<-c()
#y es la sede de la que proviene si Y=0 es de quilmes si Y=1 es de pilar 


fabrica<-Datos$fabrica
alfajores_defectusos<-Datos$defectuosos
#a) la probabilidad de que una caja provenga de la sede Quilmes.

rta_a<-sum(fabrica==0)/length(fabrica)

#b) la probabilidad de que una caja sea producida por la sede Quilmes y tenga 3 alfajores defectuosos

b<-(fabrica==0 & alfajores_defectusos==3)
rta_b<-sum(b)/length(b)

#c) la funcíon de probabilidad puntual conjunta del vector (X, Y )

table(fabrica,alfajores_defectusos)/500

#a esperanza y la varianza de X

#x<- datos defectusos
esperanza<-sum(alfajores_defectusos)/length(alfajores_defectusos)
varianza<- sum((alfajores_defectusos-esperanza)^2)/length(alfajores_defectusos)

#e) la probabilidad de que una caja producida por la sede Quilmes tenga 3 alfajores defectuosos.
rta_e<-rta_b/rta_a
#f) la probabilidad de que una caja producida por la empresa y elegida al azar tenga 3 alfajores defectuosos (es decir X = 3).

quilmes<-sum(fabrica==0 & alfajores_defectusos==3)/length(alfajores_defectusos)
pilar<-sum(fabrica==1 & alfajores_defectusos==3)/length(alfajores_defectusos)
rta_f<- quilmes+pilar

#g) la probabilidad de que una caja con 3 alfajores defectuosos haya sido producida por la sede Quilmes
#p(def3|q)=p(def y q)/p(q)
rta_g<-rta_e/rta_f

#h) la probabilidad de que una caja con 3 alfajores defectuosos haya sido producida por la sede Pilar

#EJERCICIO 2 
#plot(cumsum(c(1,2,3)))
datos_lampara<-read.table("lamparas.txt",header = TRUE)
Datos2<-datos_lampara$X26.43
#a) Estimar la probabilidad de que una l ́ampara producida por esta f ́abrica dure m ́as de 30 horas
rta_2a<-sum(Datos2>30)/length(Datos2)
#b) Hallar y graficar la funci ́on de distribuci ́on emp ́ırica para este conjunto de datos
plot(ecdf(Datos2),xlab="eje x",ylab="eje y",main="funcion distribucion")
#c) Completar: Estos datos permiten estimar que el 90 % de las l ́amparas producidas por esta f ́abrica dura m ́as de ........ horas y el 10 % dura menos de ........ horas.
quantile(Datos2,probs = c(0.1,0.5))




#EJERCICIO 3 

datos_graduados<-read.table("graduados.txt",header = TRUE)
datos_graduado<-datos_graduados$X3.46
#a) Calcular la media muestral y la mediana muestr
mean(datos_graduado)
median(datos_graduado)
#desviacion estandar 
sd(datos_graduado)
#aproximacion a funcion densidad=> histograma

hist(datos_graduado,freq = FALSE)
grilla<-seq(min(datos_graduado),max(datos_graduado),length=1000)
#normal 
fx<-dnorm(grilla,mean(datos_graduado),sqrt(var(datos_graduado)))
lines(grilla,fx)
#D

boxplot(datos_graduado)

# 4. La siguiente tabla contiene valores de poblacion, en cientos de miles, de
# las 10 ciudades mas pobladas de 4 pases en el a~no 1967. Estos datos se 
# encuentran en el archivo ciudades.txt.
datos4<-read.table("ciudades.txt",header=TRUE)
datosArg<-datos4$Argentina
datosEEUU<-datos4$EEUU
datosHolanda<-datos4$Holanda
datosJapon<-datos4$Japon

# a) Construir en paralelo, para facilitar la comparacion, un boxplot para los datos de
# cada pas e identicar los puntos extremos en cada uno de ellos.
# Comandos de R:
#   ciudades <- read.table("ciudades.txt", header = TRUE)
# View(ciudades)
# boxplot(ciudades)
View(datos4)
boxplot(datos4)

########################  Funciones  #################################
esperanzaFuncion<-function(n)
{
  a<-sum(n)/length(n)
  return(a)
}
varianzaFuncion<-function(n)
{
  varianza<- sum((n-esperanzaFuncion(n))^2)/length(n)
  return(varianza)
}
######################################################################



# b) Comparar los centros de cada poblacion, sus dispersiones y su simetra. >Cual es
# el pas mas homogeneamente habitado?

esperanzaFuncion(datosArg)
esperanzaFuncion(datosEEUU)
esperanzaFuncion(datosHolanda)
esperanzaFuncion(datosJapon)
varianzaFuncion(datosArg)
varianzaFuncion(datosEEUU)
varianzaFuncion(datosHolanda)
varianzaFuncion(datosJapon)


hist(datosArg,main = "histograma con curva de poisson")
ejeXPoisson<-seq(min(datosArg),max(datosArg),length=60)
ejeYpoisson<-dpois(ejeXPoisson,esperanzaFuncion(datosArg))
lines(ejeXPoisson,ejeYpoisson,col="red")
lines(0:60,ejeYpoisson)

hist(datosEEUU)


hist(datosHolanda)
hist(datosJapon)




# 5. El archivo ingresos.txt contiene el ingreso mensual de un conjunto de 1000 trabajadores
# registrados de una ciudad, en miles de pesos.
# a) >Cual es el ingreso mnimo percibido por los trabajadores encuestados? Estime la
# proporcion de los trabajadores de la ciudad que percibe el ingreso mnimo.

datos5<-read.table("ingresos.txt",header = TRUE)
View(datos5)
min(datos5$X15)


# b) Estimar el ingreso mensual que se necesita para pertenecer al 10% de trabajadores
# de la ciudad con ingresos mas altos.
# c) Calcular la media muestral, la mediana muestral y la media -podada con  =
#   0;10 (10 %).
# d) Calcular el desvo estandar muestral y la distancia intercuartil.
# e) Realizar un histograma y un boxplot. >Cuales son las caractersticas mas sobresalientes?
#   >Hay outliers?
#   f ) >Cree que los datos tienen distribucion normal?
#   g) Discutir con un compa~nero las ventajas y desventajas de cada medida de posicion
# para describir el centro de los datos.


#adasd
# .git ignore 


