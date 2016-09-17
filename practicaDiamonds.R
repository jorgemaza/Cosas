#	 Practica Diamonds
#
#		Jorge Maza de Julian. 
#		Programacion estadistica.
#		Master en Big Data y Businesss Analytics

#   Esta libreria, QuantPsyc, ha de ser instalada
#   ya que se  utiliza para el ultimo punto
install.packages("QuantPsyc")

#	 Hacer uso del dataset diamonds que contendra el precio (entre otras variables interesantes) de unos 54.000 diamantes.
#	
#	 Objetivo: Realizar distintos tipos de analisis estadistico de sus variables para intentar averiguar algun tipo 
#	 de comportamiento oculto aparentemente en los datos. 
#	
#	 Para ello se marcan los siguientes pasos: tipos de variables, medidas de posicion central, medidas de dispersion,
#	 distribucion y relacion entre ellas, mas analisis de regresion.
#	
#	 Los diferentes indicadores presentes en el dataset diamonds son los siguientes:
#	
#		Precio en dolares americanos (price)
#		Peso del diamante (carat)
#		Calidad del corte (cut: Fair, Good, Very Good, Premium, Ideal)
#		Color del diamante (color: desde D el mejor hasta J el peor)
#		Claridad del diamante (clarity: desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF)
#		Longitud en mm (x)
#		Ancho en mm (y)
#		Profundidad en mm (z)
#		Porcentaje total de profundidad (depth)
#		Anchura de la parte superior de diamante con relacion al punto mas ancho (table)
#	
#	 Responde cada bloque cubriendo al menos lo indicado:

library(QuantPsyc)
library(ggplot2)
dt<-as.data.frame(diamonds)

head(dt)
attach(dt)

#		Muestra representativa.
#			Selecciona una muestra representativa para cut
#	 	--> Se realizara un muestreo por estratificacion:
muestraRepre <- c()
#		-->	Se extraen los tipos de corte
cortes<-levels(dt$cut)
#	   -->	Muestreo aleatorio simple del 47% de cada estrato

listaNumCortes <- 1:length(cortes)
for(i in listaNumCortes) { 
  corte <- with(dt, cut[cut==cortes[i]])
  longitudCorte <- length(corte)
  corte<-corte[1:longitudCorte*0.25]
  muestraRepre<-c(muestraRepre,corte)
}

muestraRepre<-data.frame(muestraRepre)

boxplot(muestraRepre)

for(i in listaNumCortes){
  muestraRepre[muestraRepre==i] <- cortes[i]
}

muestraRepre<-data.frame(muestraRepre)

#	Analisis de las variables
#		Analisis descriptivo de las variables: Tipo de variable, distribucion y representacion

#	-- Precio en dolares americanos (price): 
#		Se trata de una variable cuantitativa continua que no cumple la distribucion normal. 
#		Su representacion del recuento del precio de menor a mayor en el eje de abscisas descubre que en condiciones normales 
#		va disminuyendo la los precios caros

summary(dt$price)

maxShapiro <- 5000

shapiro.test(sample(dt$price,maxShapiro,rep=F))

#	-- Peso del diamante (carat):  
#		Se trata, al igual que el precio en dolares, de una variable cuantitativa continua con distribucion no normal. 
#		Su representacion muestra un nivel de variacion menos tipico que en “price”. 

summary(dt$carat)
shapiro.test(sample(dt$carat,maxShapiro,rep=F))

# 	-- Calidad del corte (cut: Fair, Good, Very Good, Premium, Ideal): “cut” es un tipo de variable cualitativa (o categorica) nominal. 
# 		Distribucion no normal. Hay siempre mayor cantidad de elementos de cierto corte a medida que este tiene mejor calidad.

summary(dt$cut)
shapiro.test(sample(as.numeric(dt$cut),maxShapiro,rep=F))

# 	-- Color del diamante (color: desde D el mejor hasta J el peor). Variable categorica ordinal. Distribucion no normal.

summary(dt$color)
shapiro.test(sample(as.numeric(dt$color),maxShapiro,rep=F))

#	-- Claridad del diamante (clarity: desde el peor I1, SI2, SI1, VS2, VS1, VVS2, VVS1, hasta el mejor IF). 
#		Mismas caracteristicas que el color.

summary(dt$clarity)
shapiro.test(sample(as.numeric(dt$clarity),maxShapiro,rep=F))

#	-- El resto de variables son medidas / porcentajes: cuantitativas continuas.
# 		Longitud en mm (x)

summary(dt$x)
shapiro.test(sample(dt$x,maxShapiro,rep=F))

# 		Ancho en mm (y)

summary(dt$y)
shapiro.test(sample(dt$y,maxShapiro,rep=F))

#		Profundidad en mm (z)

summary(dt$z)
shapiro.test(sample(dt$z,maxShapiro,rep=F))

#		Porcentaje total de profundidad (depth)

summary(dt$depth)
shapiro.test(sample(dt$depth,maxShapiro,rep=F))

#		Anchura de la parte superior de diamante con relacion al punto mas ancho (table)

summary(dt$table)
shapiro.test(sample(dt$table,maxShapiro,rep=F))

# 	Cada distribucion ha recibido el shapiro.test. Han de tener un p-valor mayor de 0,05 (alfa = 0.95) para que se obtenga normalidad.

# 	Ninguna de ellas posee distribucion normal por lo que de ahora en adelante algunos de los analisis que requieren distribucion normal
#		se haran en base a la suposicion de que esta lo sea.

# 	Deteccion de casos atipicos y su tratamiento

# 	Se ha implementado una funcion que extrae los datos atipicos tanto leves como extremos tomando los datos de la columna del
#	 dataframe para poder posteriormente eliminarlos si fuese necesario. 

# 	Esta funcion devuelve los outliers (valores atipicos extremos)
# 	Si se quisieran extraer unicamente los atipicos extremos, habria que pasar type con valor 3

outliers <- function(data, type=1.5) {
  # Q1 es el del 25%
  q1 <- quantile(data, 0.25)
  # Q1 es el del 75%
  q3 <- quantile(data, 0.75)
  iqr <- q3 - q1
  
  # Identificar outliers
  extremoMax <- (iqr * type) + q3
  extremoMin <- q1 - (iqr * type)
  result <- which(data > extremoMax | data < extremoMin)
}

# Ejemplo con price
temp <- outliers(dt$price)

dtprice <- dt$price

porcentajeOutlier = length(temp)/nrow(dtprice)

# A continuacion lo mismo pero para todas. Se seguira el criterio de que
# si existe mas del 30% de outliers en la distribucion se traten los datos
# de la misma.
esCuantitativa <- c(T, F, F, F, T, T, T, T, T, T)
nombresColumnas <- colnames(dt)

for(col in 1:ncol(dt)){
  if(!esCuantitativa[col]){
    temp<-outliers(as.numeric(dt[,col]))
    valor0a1<-(length(as.numeric(temp))/nrow(dt[col]))
    print(nombresColumnas[col])
    print(valor0a1)
    if(valor0a1 > 0.3){     
      print("¡TIENE MaS DEL 30% DE OUTLIERS!")
    }
  } else {
    temp<-outliers(dt[,col])
    valor0a1<-(length(temp)/nrow(dt[col]))
    print(nombresColumnas[col])
    print(valor0a1)
    if(valor0a1 > 0.3){
      print("¡TIENE MaS DEL 30% DE OUTLIERS!")
    }
  }
  print("---------------------")
}

# No sale ninguno con valor mayor de 0.3


#	Inferencia

#	Calcula un intervalo de confianza para la media de carat y depth

# 	-- Planteemos un intervalo de confianza para carat al 95% tomando una muestra de 36800 diamantes.

muestra1 <- sample(dt$carat, 36800, rep=F)

mean(muestra1)

sd(muestra1)

mean(muestra1) - (1.96 * sd(muestra1) / sqrt(length(muestra1)))

#	Como ejemplificacion se muestra el siguiente caso de una de las muestras aleatorias probadas:
#
#	Un resultado tiene como media 0.7972478 y como desviacion tipica 0.4732049. 
#	Ahora hay que buscar el intervalo para p=0.95.
#
#					p = 1 - a
#					a=0.05
#	
#	Por tanto, Z sera 1.96 con media = 0.7972478 ±1.96 0.4732049/sqrt(36800)
#
#	El intervalo de confianza de una muestra representativa de 36800 diamantes para carat
#		 con p = 0.95 esta en (0.792413, 0.8020827).
#
#
#	Por otra parte, con depth, se calculara un intervalo de confianza al 90% tomando una muestra de 27000 diamantes.

muestra2 <- sample(dt$depth, 27000, rep=F)

mean(muestra2)

sd(muestra2)

mean(muestra2) - (1.645 * sd(muestra2) / sqrt(length(muestra2)))

#	Aqui otro ejemplo de una ejecucion de cierta muestra aleatoria ya ejecutada.
#
#	En el caso de otra muestra, el resultado tiene como media 61.73619 y como desviacion tipica 1.416325. 
#	Ahora hay que buscar el intervalo para p=0.90.
#	
#				p = 1 - a
#				  a=0.1
#		
#		Por tanto, Z sera 1.645
#	
#		media=0.7972478 mas/menos 1.645 0.4732049/sqrt(36800)
#
#	Asi pues, el intervalo de confianza de esta muestra representativa de 
#	36800 diamantes para carat con p = 0.95 esta en (61.72201, 61.75036).


#	Formula un test de hipotesis
#		
#	Determinar si se puede aceptar que los diamantes de la muestra tienen una media de 0.8 quilates (carat). 
t.test(muestra1,mu = 0.8,conf.level = 0.95)

#	Otro test de hipotesis seria determinar si los diamantes tienen una media del 62% de profundidad (depth).
t.test(muestra2,mu = 62,conf.level = 0.9)

#	((t.test es una funcion que realiza el test de hipotesis por t-Student))

#	Relaciones entre las variables

#	A continuacion se desvela la corrleacion entre ellas utilizando el coeficiente de que corresponda. 
#	Se ha creado una funcion que muestra los resultados de aplicar cada variable de diamonds con otra para analizar su correlacion.

#		Segun que tipo de combinacion de variables tengamos, se calculara el coeficiente por medio de Pearson, t-Sudent o ANOVA. 

#		Para ello se ha implementado un funcion a la que entran dos variables y segun sus caracteristicas son calculadas de una forma u otra.

#		Dos variables cuantitativas o una cuantitativa y otra cualitativa:
#			 Coeficiente de correlacion de Pearson. 
#
#				cor(variable1,variable2)
#			
#		Dos variables cualitativas:
#			Coeficiente de correlacion “Phi”
#			(Raiz cuadrada del resultado de la fraccion de 
#			Chi cuadrado entre el tamaño de la muestra. 
#			Resultado entre 0 y -1)
#			
#					phi=sqrt(chi^2/n), o lo que es lo mismo:
#
#		sqrt(chisq.test(table(variable1, variable2))$statistic/length(x))

arrayNCols <- c(seq(1, ncol(dt)))

correlationDataframe <- data.frame(Type<-character(),var1<-character(),var2<-character(), Correl<-numeric(),stringsAsFactors=FALSE) 

rowCor <- function(Type, var1, var2, coef) {
  newRow<-data.frame(Type, nombresColumnas[var1], nombresColumnas[var2], coef)
  newRow
}

for(var1 in 1:ncol(dt)){
  for(var2 in 1:ncol(dt)){
    # Para no repetir
    if(var2<var1){
      # Dos cuantitativas
      if (esCuantitativa[var1] && esCuantitativa[var2]) {
        # Pearson
        pea <- cor(dt[[var1]], dt[[var2]])
        newRow<-rowCor("PearsonCTCT", var1, var2, pea)
        correlationDataframe<-rbind(newRow,correlationDataframe)
        
        # Dos cualitativas
      } else if (!esCuantitativa[var1] && !esCuantitativa[var2]) {
        # Phi
        phi <- unname(sqrt(chisq.test(table(dt[[var1]], dt[[var2]]))$statistic/nrow(dt)))
        newRow<-rowCor("PHI", var1, var2, phi)
        correlationDataframe<-rbind(newRow,correlationDataframe)
        
        # Cuantiativa y cualitativa
      } else{
        # Pearson <-- Hay que convertir categoricos en numeric
        dt1 <- dt[[var1]]
        dt2 <- dt[[var2]]
        if(!esCuantitativa[var1]){
          #dt1 <- as.numeric(dt1)
          #r2 <- summary(lm(dt1 ~ dt2))$r.squared
          
          pea <- cor(as.numeric(dt[[var1]]), dt[[var2]])
          
          # A la izq la cuantitativa
          newRow<-rowCor("PearsonCTCL", var2, var1, pea)
          
          #newRow<-rowCor("R2", var2, var1, r2)
        } else{
          #dt2 <- as.numeric(dt2)
          #r2 <- summary(lm(dt2 ~ dt1))$r.squared
          
          pea <- cor(dt[[var1]], as.numeric(dt[[var2]]))
          # A la izq la cuantitativa
          newRow<-rowCor("PearsonCTCL", var1, var2, pea)
          
          #newRow<-rowCor("R2", var1, var2, r2)
        }        
        correlationDataframe<-rbind(newRow,correlationDataframe)
      }
    }
  }
}
# 	Los resultados vienen dados en el correlationDataframe creado.
correlationDataframe

# 	Por otro lado, se han hecho diversos calculos ANOVA utilizando funcion aov 
# 	(variables cuantitativas con cualitativas/categoricas).

# 	Aprovechando el anterior dataframe, como se ha utilizado el PearsonCTCL para variables numericas con categoricas, 
# 	se extrae del dataframe aquellos Type con valor PearsonCTCL para ese estudio
# 	de correlacion y se calcula su var1 con su var2.

# 	Matcheando el nombre con el array de nombresColumnas se obtiene su posición en el dataframe
# 	por lo que basta con aplicar la del dataset que es el mismo índice y extraer el ANOVA de cada uno.
ctcl<-subset(correlationDataframe,Type=="PearsonCTCL")

ctcl$nombresColumnas.var1. <- match(ctcl$nombresColumnas.var1.,nombresColumnas)
ctcl$nombresColumnas.var2. <- match(ctcl$nombresColumnas.var2.,nombresColumnas)


for(x in 1:nrow(ctcl)) {
  cuantitativa<-dt[[ctcl[x,]$nombresColumnas.var1]]
  cualitativa<-dt[[ctcl[x,]$nombresColumnas.var1]]
  anova <- aov(cuantitativa~cualitativa)
  
  s<-summary(anova)
  
  print(c("Var1 ", as.character(ctcl[x,]$nombresColumnas.var1.),"Var2 ", as.character(ctcl[x,]$nombresColumnas.var2.)))
  print(s)
}

# 	Se crea de esta forma un dataframe con los datos que nos interesan dados segun que tipo de estudio de correlacion 
# 	se deseaba teniendo en cuenta la clase de variable de las columnas (var1 y var2).

# 	--> CTCT Significa dos cuantitativas, y CTCL una cuantiativa y otra cualitativa, respectivamente <--

#	Analisis de regresion
#   Formular un modelo de regresion y analiza los resultados

#	Se estudiara la regresion del precio del diamante como variable dependiente de los valores z y carat. 

#	Los valores de Pearson en el dataframe generados son:
# 	30 PearsonCTCT                 price                 carat  0.92159130
#	3  PearsonCTCT                     z                 price		0.86124944

# 	Tomamos como variables independientes z y carat y como variable dependiente price
plot(price ~ z, dt)
plot(price ~ carat, dt)

#	plot(price ~ z+carat, dt)

# 	Se observa cierto aumento del precio segun van aumentando z y carat
lm<-lm(price ~ z + carat,dt)

lm

abline(lm,col="red")

summary(lm)

# 	Hipotesis nula: afirma que no hay efecto o relacion entre las variables.
# 	El p-valor es el valor de la probabilidad de que la hipotesis nula sea cierta.
# 	Tenemos un p-valor muy bajo por tanto rechazamos la hipotesis nula y aceptamos 
# 	que las variables guardan una correlacion, de hecho, 
# 	el valor R cuadrado es muy alto, pero con este hecho de que el valor R cuadrado sea alto
# 	no es suficiente para afirmar lo dicho, pues puede tener un p-valor alto y por tanto
# 	no tendría sentido rechazar la hipotesis nula.

# Muestra los residuos y analiza los resultados
plot(resid(lm))
abline(0,0,col="red")

# 	La grafica muestra que hay heterocedasticidad, es decir, que la distribucion 
# 	no se ajusta a este modelo debido a que la varianza es diferente entre los puntos de
# 	distribucion residual.

shapiro.test(sample(resid(lm),maxShapiro))

# 	El valor p-value es extremadamente bajo, asi se demuestra que la distribucion de residuos
# 	no es normal

# 	Aplica una transformacion a la regresion y analiza los resultados

# 	La transformacion a aplicar sera la logaritmica a cada valor
plot(log(price) ~ z,dt)
plot(log(price) ~ carat,dt)

#	plot(log(price) ~ z + carat,dt)
lmTrans<-lm(log(price) ~ z + carat,dt)
abline(lmTrans,col="red")

lmTrans

summary(lmTrans)
plot(resid(lmTrans))
abline(0,0,col="red")

# 	Sigue habiendo heterocedasticidad

# 	Por otra parte el test de normalidad de los residuos resulta demasiado bajo.
shapiro.test(sample(resid(lmTrans),maxShapiro))

#	No ha sido una transformacion fructifera

#	Interpreta los coeficientes estandarizados de la regresion

#	Para este estudio se parte del modelo inicial y se extraen los valores 
#	beta, es decir, los coeficientes estandarizados,
#	utilizado la libreria QuantPsyc instalada al inicio
lmbeta<-lm.beta(lm)

lmbeta
#	El valor resultante es la estandarizacion del valor de las constante en la regresion 
#	a escala de la desviacion tipica de y.

#	El estudio presentado de regresion lineal multiple demuestra que los valores beta
#	de z y carat son 
#	 z      	carat 
#	-0.1909236  1.1036155 

#	Lo que indica que carat tiene mayor importancia por cada variacion de una unidad en 
#	la variable dependiente.
