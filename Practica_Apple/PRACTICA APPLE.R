library(forecast)
library(xts)
library(ggplot2)
library(ggfortify)

datos<- read.csv("apple.csv")
datos
#ANALISIS EXPLORATORIO DE DATOS
dim(datos)
summary(datos)
str(datos)
head(datos)
tail(datos)
#QUITAMOS LOS NA LOS SUSTITUIMOS POR 0 Y NOS CREAMOS UNA COLUMNA CON LA SUMA DE LAS VENTAS
sum(is.na(datos))
datos[is.na(datos)]<- 0
datos

for (i in 1:nrow(datos)){
  datos$total<- datos[,3]+datos[,4]+datos[,5]+datos[,6]
}
datos
#CAMBIAMOS FORMATO DE LAS FECHAS Y PASAMOS LA COLUMNA DE TIME A FILAS
str(datos)
rawventas<- datos$total
rawdate<- seq(as.Date("1998/10/01"), as.Date("2016/01/31"), by="quarter")
xventas<- xts(rawventas, order.by = rawdate)#Dataframe especifico para fechas
xventas<- to.quarterly(xventas)
zventas<- as.zoo(xventas$xventas.Close)#El formato zoo es el que necesitamos para la visualización
View(zventas)
names(zventas)<-"Ingresos totales"

#MODELO ETS

autoplot(zventas)+ggtitle("Ventas Trimestrales APPLE")+xlab("Trimestres")+ylab("Ventas")
ggfreqplot(as.ts(zventas),freq=4,nrow=1,facet.labeller=c("1T","2T","3T","4T"))+ggtitle("Ventas Trimestrales")
cOmit=4

#Data Size
nObs=length(zventas)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),]
oventas <- window(zventas,start=index(zventas[1]),end=index(zventas[nObs-cOmit]))

#Fit Simple Exponential Smoothing
fit1 <- ses(oventas)

#Fit Holt
fit2 <- holt(oventas)

#Fit Holt- exponential
fit3 <- holt(oventas,exponential=TRUE)

#Fit Holt - damped
fit4 <- holt(oventas,damped=TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oventas,exponential=TRUE,damped=TRUE)

# Results for first model:
fit1$model
fit1$residuals

#Plot models fitted
plot(fit3, type="o", ylab="Ventas",  flwd=1, plot.conf=FALSE)
lines(window(zventas),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))



#seasonal model Holt-winters
fit6 <- hw(oventas,seasonal="additive")

fit7 <- hw(oventas,seasonal="multiplicative")

#Plot models
plot(fit7,ylab="Ventas",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(window(zventas),type="o",col="blue")
lines(fitted(fit6), col="red", lty=2)
lines(fitted(fit7), col="green", lty=2)
lines(fit6$mean, type="o", col="red")
lines(fit7$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))



#Calculate Components
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean


## Select automatic ETS
etsfit<-ets(oventas)
#forecast model
fventas.ets=forecast(etsfit)
#Results
summary(fventas.ets)

#Plot
plot(fventas.ets)
lines(window(zventas),type="o")

#Actual and Forecast
matrix(c(fventas.ets$mean[1:cOmit],zventas[(nObs-cOmit+1):nObs]),ncol=2)


## Select automatic ETS
etsfit2<-ets(oventas,damped=TRUE)
#forecast model
fventas.ets2=forecast(etsfit2)
#Results
summary(fventas.ets2)

#Plot
plot(fventas.ets2)
lines(window(zventas),type="o")

#Actual and Forecast
matrix(c(fventas.ets2$mean[1:cOmit],fventas.ets$mean[1:cOmit],zventas[(nObs-cOmit+1):nObs]),ncol=3)

#Plot all models
plot(fventas.ets2)
lines(window(zventas),type="o")
lines(fventas.ets$mean,type="o",col="red")

##MODELOS ARIMA

df_new <- data.frame(value = as.vector(zventas),
                     time = time(zventas))
ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales APPLE")+xlab("Trimestres")





#Log transformation?
zlVentas=log(zventas)
df_newl <- data.frame(value = as.vector(zlVentas),
                      time = time(zlVentas))
ggplot(df_newl)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ylab("Ventas")+ggtitle("Ventas Trimestrales LOG APPLE")+xlab("Trimestres")


#Difference
ggtsdisplay(zlVentas, main= "PRIMER GRÁFICO") #No es estacionaria
ggtsdisplay(diff(zlVentas))#No es estacionaria del todo
ggtsdisplay(diff(zlVentas,4), main="SEGUNDO GRÁFICO") #No es estacionaria (TRIMESTRES)
ggtsdisplay(diff(diff(zlVentas,4),1)) #Al principio no estacionaria y luego sí
#ACF Y PARTIAL AUTOCORRELATION AND CROSS-CORRELATION FUNCTION ESTIMATION
#TIENE COMO HIPOTESIS QUE LAS AUTOCORRELACIONES NO SON SIGNIFICATIVAS (O SEA RUIDO BLANCO)
#COMO NO SOBREPASAN LAS LINEAS AZULES LA HIPOTESIS NO SE RECHAZA, LAS AUTOCORERELACIONES SON RUIDO BLANCO
ggtsdisplay(fit1$residuals, main="Gráfico de los residuos")
ggtsdisplay(fit2$residuals)
ggtsdisplay(fit3$residuals)
ggtsdisplay(fit4$residuals, main="Segundo gráfico para los residuos")
ggtsdisplay(fit5$residuals)
ggtsdisplay(fit6$residuals)
ggtsdisplay(fit7$residuals)

#Select number of observation to compare forecast
cOmit=6

#Data Size
nObs=length(zventas)

#sub_sample
oVentas <- window(zventas,start=index(zventas[1]),end=index(zventas[nObs-cOmit]))

#out sample (real data to forecast performance)
pVentas <- window(zventas,start=index(zventas[nObs-cOmit+1]),end=index(zventas[nObs]))


#ARIMA MODEL
fit1=auto.arima(oVentas,lambda=0)
summary(fit1)

#residual analysis
ggtsdisplay(fit1$residuals, main="Análisis final de los residuos")
#box-Ljung Test
Box.test(fit1$residuals,lag=4, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=8, fitdf=3, type="Lj")
Box.test(fit1$residuals,lag=12, fitdf=3, type="Lj")

 fventas.arima=forecast(fit1)

ggplot(df_new)+geom_point(aes(x=time,y=value))+geom_line(aes(x=time,y=value))+ geom_forecast(fventas.arima,alpha=0.4)+ggtitle("ARIMA: Predicción APPLE")


fventas.arima











