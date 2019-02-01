library(openxlsx)
library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)
library(TSA)
library(lmtest)

weeklyData <- to.weekly(d.data)
zWeeklyData <- as.zoo(weeklyData$d.data.Close)
names(zWeeklyData) <- 'Semanal'

#Una vez convertido a semanal graficamos, en donde el eje de las x serán las semanas y el eje de las Y serán las ventas
#Vemos a simple que la serie tiene varios picos. Nos encontramos ante un impulso y no un escalón , ya que la tendencia de la serie se recupera en 
#cuanto a su media y varizanza. De cara a realizar el análisis de intervención tendremos que ver que patrón polinómico es el que sigue
autoplot(zWeeklyData) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota semanal") + xlab("Semanas") + 
  ggtitle('Representacion semanal')


monthlyData <- to.monthly(d.data)
zMonthlyData <- as.zoo(monthlyData$d.data.Close)
names(zMonthlyData) <- 'Mensual'

#Vamos a continuar con nuestro análisis para ver cómo se comporta la serie temporal cuando la convertimos en mensual.
#Al igual que el gráfico anterior el eje de las x representará los meses y el eje de coordenadas las ventas
#Exiten diferencias importantes entre las dos series. En la serie mensual observamos que antes del año 2010 experimentó un crecimiento de
#ventas bastante considerado, sin embargo durante el resto de los años las ventas se han recuperadotanto en media como en varianza, por lo que
#al igual que en la serie temporal por semanas nos encontraríamos también ante un impulso y no un escalón. De nuevo, para realizar el 
#análisis de intervencióntendremos que ver el patrón polonómico que sigue.
autoplot(zMonthlyData) + geom_point() +
  ylab("Ventas") + ggtitle("Cuota mensual") + xlab("Semanas") + 
  ggtitle('Representacion mensual')

cOmit = 0
nObsWeek = length(zWeeklyData)
nObsMonth = length(zMonthlyData)

oVentasWeekly <- window(zWeeklyData, start = index(zWeeklyData[1]), end = index(zWeeklyData[nObsWeek - cOmit]))
oVentasMonthly <- window(zMonthlyData, start = index(zMonthlyData[1]), end = index(zMonthlyData[nObsMonth - cOmit]))

#El modelo ETS descompone los datos en sus partes componentes y los extiende a futuro para pronosticar. En cambio, con el modelo ARIMA 
#variaciones y regresiones de datos estádisticos con el fin de encontrar patrones para una predicción hacia el futuro

#Vamos a ver, como una primera aproximación, los resultados arrojados a través del modelo ARIMA

fit.semanal = auto.arima(oVentasWeekly,lambda=0, ic = "aic") 
summary(fit.semanal) # Para la serie temporal semanal tenemos un ARIMA de(0,1,0), no teniendo componente estacional  

fit.mensual = auto.arima(oVentasMonthly,lambda=0, ic = "aic")
summary(fit.mensual) #Para la serie temporal mensual tenemos un arimaARIMA(2,0,2)(2,0,0), en donde a diferencia del primero, sí que tiene componente estacional

#Vamos a estudiar como se comportan los residuos
#A continuación realizamos el análisis de los residuos con la función de autocorrelación simple (ACF) 
#y con la función de autocorrelación parcial (PACF), viendo que no se salgan de las franjas azules.
ggtsdisplay(fit.semanal$residuals)
#Hacemos lo mismo para los meses
ggtsdisplay(fit.mensual$residuals)



checkresiduals(fit.mensual)
#Vemos que los residuos para los meses se comportan como una distribución normal

checkresiduals(fit.semanal)
#El análisis para el semanal también podemos decir que los residuos se comportan como una distribución normal, aunque no existe
#tanto pronunciamiento y los resultados no son tan concluyentes como con el análisis de los residuos para la serie mensual


#Si bien con el impacto visual veíamos que los residuos se comportaban como ruido blanco, estadísticamente a travésdeltestBox-Ljung, 
#quenoesmásqueuntipodepruebaestadísticadesiungrupodeautocorrelaciones de una serie temporal son diferentes de cero. La hipotesis nula 
#asume que existe ruido blanco.

#Con un p-valor muy superior al nivel de signiﬁcación del 5% no podemos rechazar la hipótesis nula, corroborando la información proporcionada
#por los residuos, comportándose como tales como ruido blanco. 

#Los residuos para la serie temporal mensual son ruido blanco
Box.test(fit.mensual$residuals,lag = 3, fitdf = 1, type = "Lj")
Box.test(fit.mensual$residuals,lag = 6, fitdf = 1, type = "Lj")
Box.test(fit.mensual$residuals,lag = 9, fitdf = 1, type = "Lj")


#El análisis para los residuos no arroja resultados concluyentes
Box.test(fit.semanal$residuals,lag = 3, fitdf = 1, type = "Lj")
Box.test(fit.semanal$residuals,lag = 6, fitdf = 1, type = "Lj")
Box.test(fit.semanal$residuals,lag = 9, fitdf = 1, type = "Lj")


#Realizamos la predicción para los meses, como en este caso nos están preguntando sobre la previsión según el modelo ARIMA en Agosto 2018
#Nuestra h será igual a 1
fmensual.arima=forecast(fit.mensual, h=1)
plot(fmensual.arima)
fmensual.arima
#Para Julio teníamos unas ventas de 950, con nuestra predicción para Agosto de 2018 aumentarán las mismas hasta alcanzar un 1653,32



#Realizamos la predicción para las semanas, como en este caso nos están preguntando sobre la previsión según el modelo ARIMA en Agosto 2018
#nuestra h será igual a 4
fsemanal.arima=forecast(fit.semanal, h=4)
plot(fsemanal.arima)
fsemanal.arima
#Según la predicción semanal observamos que para las 4 semanas del mes de Agosto serán las mismas.

#Vamos a realizar un análisis de los outliers
#Los outliers pueden crear estructuras de autocorrelación espurias, que se evitan añadiendo componentes de intervención. 
#Existen dos tipos de outliers, los aditivos (AO), los cuales afectan a la serie temporal; y los innovativos (IO), 
#los cuales afectan a los residuos

detectAO(fit.mensual) #No existen outliers aditivos para la serie mensual
detectIO(fit.mensual) #No existen outliers innovativos para la serie mensual


detectAO(fit.semanal) #No existen outliers aditivos para la serie semanal
detectIO(fit.semanal) #Existen muchos outliers para los residuos de la serie semanal, dicho de otro modo existen outliers innovativos


#Utilizaremos el modelo ETS como método de predicción, para ambas series, ya que las dos son impulso y las dos son estacionarias


fit1 <- ses(oVentasWeekly)

#Fit Holt
fit2 <- holt(oVentasWeekly)

#Fit Holt- exponential
fit3 <- holt(oVentasWeekly,exponential = TRUE)

#Fit Holt - damped
fit4 <- holt(oVentasWeekly,damped = TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oVentasWeekly,exponential = TRUE,damped = TRUE)

# Results for first model:
fit1$model
fit1$residuals

library(plotly)
plotly(plot(fit3, type = "o", ylab = "Ventas",  flwd = 1, plot.conf = FALSE))
lines(window(zVentas), type = "o")
lines(fit1$mean, col = 2)
lines(fit2$mean, col = 3)
lines(fit4$mean, col = 5)
lines(fit5$mean, col = 6)
legend("topleft", lty = 1, pch = 1, col = 1:6,
       c("Data","SES","Holt's","Exponcial",
         "Additive Damped","Multiplicative Damped"))

#El efecto multiplicativo se presenta cuando el patrón estacional en los datos depende de su tamaño, es decir, cuando la magnitud del patrón estacional se
#incrementa conforme los valores aumentan y decrece cuando los valores de los datos disminuyen.
#Sin embargo, el efecto aditivo es mejor cuando el patrón estacional en los datos no depende del valor en los datos, es decir, que el patrón estacional no
#cambia conforme la serie se incrementa o disminuye de valor

#seasonal model Holt-winters
fit6 <- hw(oVentas,seasonal="additive")
fit7 <- hw(oVentas,seasonal="multiplicative")

#Los métodos de suavizamiento exponencial fueron sugeridos por C.C. Holt en 1957. A su vez, el método Winters se aplica en series temporales
#cuando presenta patrones de tendencia y estacionalidad.

#Plot models
plot(fit7,ylab="Ventas",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(window(zVentas),type="o",col="blue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))


etsfit<- ets(oVentasWeekly, ic = "aic") # Estimacion del modelo automatica
etsfit2 <- ets(oVentasMonthly, ic = "aic")


etsfit
#Tenemos una ETS para las semanas de (A,N,N), lo cual quiere decir que existe error aditivo, no existe tendencia ni tampoco componente estacional

#Para nuestra ETS mensual tenemos error multiplicativo, al igual que en el anterior, no existe tendencia ni componente estacional
etsfit2 


#Realizamos nuestra predicción para las semanas
fsemanal.ets = forecast(etsfit, h = 4) 
summary(fsemanal.ets)
plot(fsemanal.ets)


#A diferencia del modelo ARIMA, las previsiones de ventas semanales para Agosto son mucho más optimistas, teniendo una previsión alcista de ellas
#Sin embargo, serán las mismas para las cuatro semanas de Agosto, sim embargo con la representación gráfica no arroja resultados concluyentes


#Realizamos el mismo calculo para los meses
fmensual.ets = forecast(etsfit2, h = 1)
summary(fmensual.ets)
plot(fmensual.ets)
# La previsión de ventas según el modelo ETS es mucho más optimista igualmente para el mes de Agosto, frente a la previsión ARIMA de 1653 en ventas
#con el modelo ETS establece que la previsión en ventas para el mes de Agosto será todavía mucho mayor hasta alcanzar los 1859 en ventas


