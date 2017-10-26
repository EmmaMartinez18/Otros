###   ***     MODELO DE REGRESION MULTIPLE    ***
###   3 variables (precio de unas mochilas)
###   Varaibles independientes son la capacidad (tamaño) y la calidad de la mochila
###   la variable dependiente es el precio de la mochila.


###   var1 capacidad (tamaño)
capa <- c(4330,5500,5550,4700,5200,5500,4700,5500,5800,5000) 

###   var2 calidad
calidad <- c(2,3,4,3,4,4,4,5,5,5)

###   var3 (Dependiente)
precio <- c(190,219,249,249,250,340,289,395,439,525)

###   Se declara un dataframe con las tres variables
mochil <- data.frame(precio, capa, calidad)

###   Un elemento importante en el mrm es concer las correlaciones entre las
###   variables, en este caso para la correlacion se utiliza la funcion cor()
cor(mochil, use = "everything", method = "pearson")

###   Una vez observada las correlaciones en la que se muestra mayor correlacion
###   del precio con la calidad, aplicamos el modelo
modmo <- lm(precio ~ calidad + capa) 
summary(modmo)

#   NOTA
### Variabilidad del modelo de regresion R-Estandar
### Grados de libertad R-Ajustada


###   Se revisa la r2 ajustada, debido a que es un mrm y la r2 ajustada
###   tiene un valor de .70, lo que implica que la recta de regresion
###   Explica en 70% la variabilidad del modelo
###   Despues de que tengamos una r2 ajustada aceptable, se analiza el 
###   estadistico F para saber la bondad de ajuste y la calidad del modelo
###   que estamos aplicando

anova(modmo)

###   Una vez que aplicamos anova se va a revisar los valores del estadistico F
###   y los p-value con la finalidad de conocer la bondad de ajuste del mrm en 
###   en este caso se observa que en la var indep calidad se tiene un estadistico 
###   F de 22.49, lo que es un buen estadistico F debido a que se esperan valores
###   mayores a 1, para los p-value se espera un p-value < .05 en este caso la 
###   primer var indep cumple con un estadistico F aypr a 1 y un p-value < .05 
###   con lo que podriamos decir que la var calidad es significativa en nuestro 
###   modelo.
###   Para el analisis de la variable capacidad, el estadisicp F es 1.3 y un 
###   p-value de 0.28, en este caso podriamos concluir que esta variable no
###   resulta significativa para nuestro modelo, debido a que el estadistico 
###   F es apenas mayor a 1 y el p-value es mayor a .05

###   Despues se realiza el diagnostico de nuestro modelo a traves de las pruebas
###   formales de shapiro, breusch pagan y Durbin watson

###   Para aplicar esta prueba se neceita conocer los valores ajustados, los 
###   residuos del modelo y los residuos estudentizados
names(modmo)

mochil$fitted.modmo <- fitted(modmo)
mochil$residuals.modmo <- residuals(modmo)
mochil$rstudent.modmo <- rstudent(modmo)

###   Diagnostico de los supuestos 
###   Supuesto 1: Normalidad
###   Para el supuesto de normalidad se aplica la prueba de shapiro en r la fn.
###   que aplica esta prueba es shapiro.test

shapiro.test(mochil$rstudent.modmo)

###   En esta prueba esperamos un p-value mayor a .05 y en este caso obtiene un
###   p-value mayor a .05 por lo que podriamos decir que se acepta el supuesto de 
###   normalidad

###   Ademas de Shapiro test se debe observar graficamente esta prueba a traves
###   de las funciones de qqnorm que nos permite hacer un grafico qq

qqnorm(mochil$rstudent.modmo)
qqline(mochil$rstudent.modmo)

###   Homocedasticidad supuesto
install.packages("lmtest")
require (lmtest)
bptest(modmo)

###   Supuesto de independencia o autocorrelacion 
###   En este caso se aplica la prueba durbin watson, como lo vimos la clase 
###   anterior esta prueba se evalua de dos formas, por un lado, el valor de 
###   Durbin Watson tiene que estar entre 1.5 y 2.5 ademas de este valr hay que
###   observar el p-value que tiene que ser mayor a .05 para que cumpla con el
###   supuesto de independencia de los residuos

dwtest(modmo,alternative = "two.sided")

###   en este caso obtenemos un durbin watson de 1.87 por lo que podemos decir 
###   que cumple el supuesto de independencia y al cumplir las 3 pruebas formales
###   podriamos decir que tenemos un buen modelo y podriamos platear la ecuacion
###   del modelo.

coefficients(modmo)

###   Platear ecuacion 

y = B0 + B1 * x1 + B2 * x2

###   *** EJERCICIO EN CLASE
###   En una financiera se quiere evaluar a los corredores de bolsa de diferentes
###   empresas, la oferta y desempeño de cada corredor se evalua en 6 areas, empleado
###   paracada una de las areas una escala de 0 a 5. tres de las areas evaluadas
###   son ejecucion en la operacion, facilidad de uso y gama de ofertas.
###   Un 5 es la mejor calificacion en las diferentes areas.
###   En los siguientes datos se representan las puntuaciones obtenidas:

corredor <- c("wall st","etrade1","etrade2","pre-trade","track","water house",
              "brown","br america","merill lynch","Strogn funds")
operacion <- c(3.7,3.4,2.5,4.8,4,3,2.7,1.7,2.2,1.4)
uso <- c(4.5,3,4,3.7,3.5,3,2.5,3.5,2.7,3.6)
gama <- c(4.8,4.2,4,3.4,3.2,4.6,3.3,3.1,3,2.5)
califtotal <- c(4,3.5,3.5,3.5,3.5,3.5,3,3,2.5,2)

###   a)  Determine la ecuacion de regresion estimada que se puede usar para predecir
###       la calificacion total segun las evaluaciones obtenidas

base <- data.frame(operacion,uso,gama,califtotal)
cor(base, use = "everything" , method = "pearson")

modm <- lm(operacion ~ uso + gama + califtotal) 
summary(modm)

anova(modm)

###   b)  Emplee la prueba F para determinar la significacia global de la relacion
###       el nivel de significancia que se requiere es de 95% ¿Cual es su 
###       conclusion?

###   c)  Elimine cualquiera de las variables independietes que no sea significativa 
###       para la ecuacion de regresion estimada ¿Cual es la ec. de regresion estimada 
###       que recomienda compare las r2 de los modelos?
