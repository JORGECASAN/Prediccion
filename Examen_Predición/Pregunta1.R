
#PREGUNTA 1 EXAMEN PREDICCIÓN

library(dplyr)
require(cluster)
require(fpc)
require(factoextra)
require(dplyr)
library(dendextend)
require(ggrepel)
library(MASS)
require(NbClust)
library(memisc)
library(haven)
library(foreign)
library(dplyr)
library(factoextra)
library(cluster)
library(factoextra)
require(clustertend)
library("NbClust")
library(FactoMineR)
library(ggplot2)

##Tras cargar las librerías que vamos a emplear, se procede con la carga y limpieza de datos.

datos <- read.csv('EGT2010_2017.csv')
datos_filtrados <- datos[, c(81:93, 3, 4, 6, 21, 72, 112, 110, 109, 80, 151, 146)]
which(is.na(datos_filtrados))
datos_filtrados <- na.omit(datos_filtrados)


datos_filtrados$VALORACION_MEDIO_AMBIENTE <- ifelse(as.integer(datos_filtrados$VALORACION_MEDIO_AMBIENTE) < 8, 0, 1)

summary(datos_filtrados)

##Con los datos limpios y sin NAs se procede a separar en dos periodos diferentes (2010-2014 y 2015-2017). De estos dos periodos,
#se extraen muestras aleatorias de 150 observaciones para cada periodo. Puesto que hay que hacer una regresión logística, directamente 
#se utilizará la muestra correspondiente al primer periodo como TRAIN y la muestra del segundo periodo como TEST

set.seed(1234)

primer_df <- datos_filtrados %>% filter(datos_filtrados$AÑO <= 2014)
segundo_df <- datos_filtrados %>% filter(datos_filtrados$AÑO > 2014)

train1 <- primer_df[sample(nrow(primer_df), 150), ]
test1 <- segundo_df[sample(nrow(segundo_df), 150),]

##Eliminamos objetos innecesarios para trabajar mejor
rm(datos)
rm(primer_df)
rm(segundo_df)


########### A partir de ahora vamos a hacer un modelo de regresión linal para predecir VALORACION_MEDIO_AMBIENTE ###########

#Se elimina ID y se pasa la variable a predecir a factor
train1$VALORACION_MEDIO_AMBIENTE <- as.factor(train1$VALORACION_MEDIO_AMBIENTE)
test1$VALORACION_MEDIO_AMBIENTE <- as.factor(test1$VALORACION_MEDIO_AMBIENTE)
train1$ID <- NULL
test1$ID <- NULL



#Se hace un modelo de regresion lineal con todas las variables que tenemos en el dataset.
modelo_training <- glm(VALORACION_MEDIO_AMBIENTE ~ ., train1, family = 'binomial')
summary(modelo_training)

#Con los resultados obtenidos del modelo de regresion glm, se procede a hacer un stepAIC para averiguar cual es el modelo con 
#más variables estadísticamente significativas.
stepAIC(modelo_training, direction = 'both')

#Con un AIC de 62, elegimos las variables del mejor modelo y mejoramos el modelo anterior.
modelo_bueno_training <- glm(formula = VALORACION_MEDIO_AMBIENTE ~ VALORACION_ALOJ + VALORACION_TRATO_ALOJ + 
                               VALORACION_GASTRONO_ALOJ + VALORACION_CLIMA + VALORACION_ZONAS_BANYO + 
                               VALORACION_PAISAJES + VALORACION_TRANQUILIDAD + VALORACION_LIMPIEZA + 
                               VALORACION_CALIDAD_RESTAUR + VALORACION_OFERTA_GASTR_LOC + 
                               VALORACION_TRATO_RESTAUR + VALORACION_PRECIO_RESTAUR + AEROPUERTO + 
                               PAIS_RESID_AGRUP + NOCHES_PERNOCTADAS + INGRESOS + EDAD + 
                               SEXO + AÑO, family = "binomial", data = train1)
summary(modelo_bueno_training)

#Realizamos el modelo anova para saber cuales son las variables que mejor explican el modelo de regresión. Observamos que las que mejor
#explican el modelo son VALORACION_LIMPIEZA, VALORACION_TRANQUILIDAD y, un poco peor, VALORACION_TRANQUILIDAD
anova(modelo_bueno_training, test = "Chisq")

modelo_anova <- glm(VALORACION_MEDIO_AMBIENTE ~ VALORACION_PAISAJES+VALORACION_TRANQUILIDAD+VALORACION_LIMPIEZA+AÑO,
                    family = 'binomial', data = train1)

modelo_completo <- glm(VALORACION_MEDIO_AMBIENTE ~ ., family = 'binomial', data= train1)
summary(modelo_completo)


#Comparando todos los modelos calculados, el modelo que mejor explica es aquel con menos AIC y BIC y es el nuestro modelo denominado modelo_bueno_trainning

AIC(modelo_bueno_training,modelo_anova, modelo_completo)
BIC(modelo_bueno_training,modelo_anova, modelo_completo)


#Con el fin de determinar outliers, se realiza la matriz de distancias de Cooks. Como se observa, hay tres puntos que son outliers;
#sin embargo, esta técnica no nos dice como afectan al modelo. Puesto que solo hay tres puntos, seguimos con el modelo.
#A través de la distancia Cook (D-estadístico) ayuda a identificar los valores influyentes pero no indican como afectan al modelo
#Para cada regresor Xk, se grafican los residuos de la regresión sobre todos los regresores menos Xk y los residuos de la regresión de Xk 
#con los otros regresores.

cooksd <- cooks.distance(modelo_bueno_training)
plot(cooksd, pch='*', cex = 2, main = 'Observaciones influyentes por distancia de Cook')
abline(h = 4*mean(cooksd, na.rm=T), col="red")  
#add cutoff line	
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")



#Realizamos la predicción.
library(psych)
describe(datos_filtrados$VALORACION_MEDIO_AMBIENTE)
prediccion <-predict(modelo_bueno_training, test1, type = 'response')
prediccion

#Ahora, con una matriz de confusión se comprobara cómo de bueno es el modelo predictivo que hemos creado.
#Los resultados que nos arroja no son del todo buenos; esto se debe a que hemos trabajado con una muestra de 150 observaciones para el train
#y otra del mismo tamaño para el test. Por eso, como cada muestra corresponde a un periodo temporal diferente y además son muy pequeñas y 
# están desbalanceadas, la predicción no es del todo fiable.

library(caret)
#Consideramos el cutt-off como 0.125 puesto que desde nuestro punto de vista es el óptimo de cara a predecir con la matriz de confusión los 
#falsos positivos y falsos negativos.
#el objetivo es minimizar el coste total en el conjunto de entrenamiento.

searchgrid = seq(0.01, 0.8, 0.02)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi){
  weight1 = 1
  weight0 = 1
  c1 = (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
modelo <- glm(VALORACION_MEDIO_AMBIENTE~.,family = binomial, train1); 
for(i in 1:length(searchgrid)) {
  pcut <- result[i,1]
  result[i,2] <- cv.glm(data = train1 ,glmfit = modelo_bueno_training, cost = cost1, K=5)$delta[2]
}

result[which.min(result[,2]),]

mean(ifelse(prediccion >= 0.125, 1, 0) == test1$VALORACION_MEDIO_AMBIENTE)
mean(ifelse(prediccion >= 0.125, 1, 0) != test1$VALORACION_MEDIO_AMBIENTE)

matrizBuena <- table(ifelse(prediccion >= 0.125, 1, 0), test1$VALORACION_MEDIO_AMBIENTE)
matrizBuena
#Nuestra matriz de confusión nos arroja buenos resultados en donde la cantidad de falsos positivos son solamente 10 observaciones y la cantidad
#de falsos negativos de 26 observaciones frente a los verdaderos positvos de 27 observaciones y verdaderos negativos de 87 observaciones, lo cual quiere decir
#que el porcentaje de la predicción para la valoración del medio ambiente en las Islas Canarias es bajo, siendo el error en un 24% con una precisión, 
#por dende del 76%.

accuracyMatriz <-sum(diag(matrizBuena)) / sum(matrizBuena)
accuracyMatriz
mean(ifelse(prediccion >= 0.68, 1, 0) != test1$VALORACION_MEDIO_AMBIENTE)
#Solamente nos devuelve un porcentaje sobre los errores predichos del 24%, por lo que consideramos nuestra predicción buena.
#Obtenemos un buena precisión siendo superior al 70%



##CurvaROC
#ROC es un gráfico bidimensional en el cual la proporción de verdaderos positivos rate se representa en el eje Y y la proporción de falsos
#negativos se grafica en el eje X.
#La linea recta diagonal representa la situación de seleccionar de forma aleatoria.
library(verification)
library(ROCR)
roc.plot(test1$VALORACION_MEDIO_AMBIENTE == 1,prediccion)
roc.pl ot(test1$VALORACION_MEDIO_AMBIENTE == '1', prediccion)$roc.vol

roc <- prediction(predict(modelo_bueno_training, test1), test1$VALORACION_MEDIO_AMBIENTE)
AUC <- ROCR::performance(roc, "auc") 
AUC@y.name  
AUC@y.values 
perf <- ROCR::performance(roc, "tpr", "fpr") 
plot(perf, colorize = TRUE) 
text(0.4, 0.6, paste(AUC@y.name, "\n", round(unlist(AUC@y.values), 3)), cex = 0.7)
#El área por debajo de la curva es de 0,77. Podemos decir que cuanto mayor es el AUC mejor será la prueba.
#El área bajo la curva es el estadístico por excelencia para medir la capacidad discriminante de la prueba 


##################MODELOS DE REGULARIZACIÓN############################################

# Vamos a realizar a continuación los modelos de regularizacion con el objetivo de reducir las estimaciones de los coeficientes 
#teniendo como efecto
#que se reduzca sifnificativamente su varianza. Para ello emplearemos las dos técnicas más conocidas para reducir las 
#estimaciones de coeficientes hacia
#cero son la regresión de cresta (Ridge) y el lazo (Lasso)


###########MODELO RIDGE###############################

#La regresión Ridge es similar a los mínimos cuadrados, excepto que los coeficientes se estiman minimizando una cantidad ligeramente diferente.
#La regresión de Ridge, como OLS, busca estimaciones de coeficientes que reducen el RSS, sin embargo, también tienen una penalización por contracción
#cuando los coeficientes se acercan a cero.
#Esta penalización tiene el efecto de reducir las estimaciones del coeficiente hacia cero.
#La regresión de Ridge funciona mejor en situaciones donde las estimaciones de mínimos cuadrados tienen alta varianza. La regresión de Ridge es
#mucho más eficiente computacionalmente que cualquier método de subconjunto, ya que es posible resolver simultáneamente todos los valores de λ.


library(rsample)  # data splitting 
library(glmnet)   # implementing regularized regression approaches
library(dplyr)    # basic data manipulation procedures
library(ggplot2)  # plotting

##PARA REALIZAR ESTOS MODELOS LAS VARIABLES DEBEN DE ESTAR ESCALADAS

train1_sca <- scale(train1)

performScaling = T
if (performScaling) {
  # Loop sobre cada columna
  for (colName in names(train1)) {
    # Comprueba si la columna es de datos numÃ©ricos.
    if(class(train1[,colName]) == 'integer' | class(train1[,colName]) == 'numeric') {
      # escala la columna.
      train1[,colName] = scale(train1[,colName])
    }
  }
}


if (performScaling) {
  # Loop sobre cada columna
  for (colName in names(test1)) {
    # Comprueba si la columna es de datos numÃ©ricos.
    if(class(test1[,colName]) == 'integer' | class(test1[,colName]) == 'numeric') {
      # escala la columna.
      test1[,colName] = scale(test1[,colName])
    }
  }
}

str(train1)
##MODELOS DE REGULARIZACIÓN

ames_train_x <- model.matrix(VALORACION_MEDIO_AMBIENTE ~ VALORACION_ALOJ + VALORACION_TRATO_ALOJ + 
                               VALORACION_GASTRONO_ALOJ + VALORACION_CLIMA + VALORACION_ZONAS_BANYO + 
                               VALORACION_PAISAJES + VALORACION_TRANQUILIDAD + VALORACION_LIMPIEZA + 
                               VALORACION_CALIDAD_RESTAUR + VALORACION_OFERTA_GASTR_LOC + 
                               VALORACION_TRATO_RESTAUR + VALORACION_PRECIO_RESTAUR + AEROPUERTO + 
                               PAIS_RESID_AGRUP + NOCHES_PERNOCTADAS + INGRESOS + EDAD + 
                               SEXO + AÑO, data = train1)
ames_train_y <- log(as.integer(train1$VALORACION_MEDIO_AMBIENTE))

ames_test_x <- model.matrix(VALORACION_MEDIO_AMBIENTE ~ VALORACION_ALOJ + VALORACION_TRATO_ALOJ + 
                              VALORACION_GASTRONO_ALOJ + VALORACION_CLIMA + VALORACION_ZONAS_BANYO + 
                              VALORACION_PAISAJES + VALORACION_TRANQUILIDAD + VALORACION_LIMPIEZA + 
                              VALORACION_CALIDAD_RESTAUR + VALORACION_OFERTA_GASTR_LOC + 
                              VALORACION_TRATO_RESTAUR + VALORACION_PRECIO_RESTAUR + AEROPUERTO + 
                              PAIS_RESID_AGRUP + NOCHES_PERNOCTADAS + INGRESOS + EDAD + 
                              SEXO + AÑO, data = test1)
ames_test_y <- log(as.integer(test1$VALORACION_MEDIO_AMBIENTE))


#vamos a realizar en primerlugar una representación gráfica del modelo Ridge en donde a medida que aumenta el lambda, el error cuadrático medio aumenta
#El punto de corte está en -2 aproximadamente , sabiendo que cuando se cambia de pendiente es cuado tenemos que elegir el valor, por ser el estadísticamente
#más significativo.

#Es importante tener en cuenta que con este modelo se seleccionan todas las variables, a diferencia del Lasso.

ames_ridge <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 0
)
coef(ames_ridge)
plot(ames_ridge, xvar = "lambda")


predictRidge <- predict(ames_ridge,  type = 'coefficients')
predictRidge



mod_ridge <- cv.glmnet(x = ames_train_x, y = ames_train_y, alpha = 0)
plot(mod_ridge)
mod_ridge$lambda %>% head()
#Cada línea represeta cada variable incluida en nuestro modelo


min(mod_ridge$cvm) #Minimo error
mod_ridge$lambda.min #lambda para ese error imo

#Según el método de contracción Ridge, el mínimo error cuadrático medio es 0.06257731 y el lambda mínimo es 0.1431564

mod_ridge$cvm[mod_ridge$lambda == mod_ridge$lambda.1se] 
mod_ridge$lambda.1se  
#Nuestro lambda que minimiza el error cuadrático medio estará dentro del intervalo de lambda 0.06619203 y 0.5779248

mod_ridge_min <- glmnet(x = ames_train_x, y = ames_train_y, alpha = 0)
plot(mod_ridge_min, xvar = "lambda")
abline(v = log(mod_ridge$lambda.min), col = "red", lty = "dashed")


coef(ames_ridge) %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)

#Valoración_Limpieza, Pais_resisencia_Esaña y Mujer





coef(mod_lasso, s = "lambda.1se") %>%
  tidy() %>%
  filter(row != "(Intercept)") %>%
  top_n(25, wt = abs(value)) %>%
  ggplot(aes(value, reorder(row, value))) +
  geom_point() +
  ggtitle("Top 25 influential variables") +
  xlab("Coefficient") +
  ylab(NULL)


#Valoración_Limpieza y Paisajes

coef(ames_lasso)

###########MODELO LASSO###############################
#La regresión Ridge tenía al menos una desventaja; incluye todos los predictores p en el modelo final. 
#El término de penalización establecerá muchos de ellos cerca de cero, pero nunca exactamente a cero.
#Lasso supera esta desventaja y es capaz de forzar algunos de los coeficientes a cero, dado que s es 
#lo suficientemente pequeño. Como s = 1 da como resultado una regresión OLS regular, cuando s se acerca a 0,
#los coeficientes se reducen a cero. Por lo tanto, la regresión de Lasso también realiza la selección de variables
#Como resultado, el modelo Lasso tiende  a generar modelos más fáciles de interpretar

ames_lasso <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
) 

plot(ames_lasso, xvar = "lambda")
#De nuevo, cada línea es la prepresentación de cada variable incluida en nuestro modelo

mod_lasso <- cv.glmnet(x = ames_train_x, y = ames_train_y, alpha = 1)
plot(mod_lasso)
mod_lasso$lambda %>% head()


min(mod_lasso$cvm) #Minimo error es de 0.0581065
mod_lasso$lambda.min #lambda de 0.01499729

#Según el método de contracción Lasso, el mínimo error cuadrático medio es 0.0581065 y el lambda mínimo es 0.04173085

mod_lasso$cvm[mod_lasso$lambda == mod_lasso$lambda.1se] 
mod_lasso$lambda.1se  
#Nuestro lambda que minimiza el error cuadrático medio estará dentro del intervalo de confianza de lambda 0.06472364 y 0.08003602

mod_lasso_min <- glmnet(x = ames_train_x, y = ames_train_y, alpha = 1)
plot(mod_lasso_min, xvar = "lambda")
abline(v = log(mod_lasso$lambda.min), col = "red", lty = "dashed")


###########COMPARACIÓN MODELO RIDGE, LASSO Y ELASTIC NET###############################
#Este modelo es una combinación de los dos modelos anteriores, en donde incorpora la selección de variables del Lasso 
#y la contracción de los predictores correlacionados como la regresión del Ridge
#Representamos 4 gráficos para tener una visión global de los modelos calculados anteriormente pero incorporando un alfa de 0.25 y 0.75. 

elastic1 <-glmnet(x=ames_train_x, y = train1$VALORACION_MEDIO_AMBIENTE, family = "binomial", alpha=0.75)
elastic2 <-glmnet(x=ames_train_x, y = train1$VALORACION_MEDIO_AMBIENTE, family = "binomial", alpha=0.25)


par(mfrow = c(2, 2), mar = c(6, 4, 6, 2) + 0.1)
plot(ames_lasso, xvar = "lambda", main = "Lasso (Alpha = 1)\n\n\n")
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")
plot(ames_ridge, xvar = "lambda", main = "Ridge (Alpha = 0)\n\n\n")


#No hay un algoritmo dominante presente aquí, en general, es mejor probar las tres técnicas introducidas hasta ahora y elegir la que mejor 
#se adapte a los datos usando estimaciones de error de prueba con validación cruzada.















