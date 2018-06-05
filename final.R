install.packages("forecast")
library(forecast)
library('astsa')
library("lmtest")

#Setting Working directory
setwd("C:/Users/Ajay/Desktop/fourth/ECON")

########### Load Data Set###################################
library(readxl)
temp<- read_excel("C:/Users/Ajay/Desktop/fourth/ECON/Project/Gasoline-Ontario/monthly-gasoline-demand-ontario.xlsx")
View(temp)

########### Hold out
gasoline=ts(temp[,2],start=c(1960,1),frequency = 12)
hold=ts(temp[,2],start=c(1960,1),end=c(1975,8),frequency = 12)


###########  PLOT DATA
plot(gasoline)
plot(decompose(gasoline))

###########1: Check for stationarity:
library(urca)

summary(ur.df(hold,type="drift",lags=20,selectlags = "AIC"))           # not stationary
#*********Taking only log transformation of hold
holdLOG=log(hold)
plot(holdLOG)

summary(ur.df(holdLOG,type="drift",lags=20,selectlags = "AIC"))        # not stationary
#*********Taking first Difference of hold Log
gasoline_diff = diff(log(hold))
plot(gasoline_diff)

summary(ur.df(gasoline_diff,type="drift",lags=20,selectlags = "AIC"))  # stationary

###########2: Guessing about the model based on acf and pacf
acf(gasoline_diff,30)
pacf(gasoline_diff,30)

###########3: Estimate models
Arima(holdLOG,order=c(1,1,1),seasonal=c(0,0,1))                                #BIC=-455.29
Arima(holdLOG,order=c(1,1,1),seasonal=c(1,0,1))                                #BIC=-633.72
Arima(holdLOG,order=c(2,1,1),seasonal=c(1,0,1))                                #BIC=-662.90
Arima(holdLOG,order=c(2,1,2),seasonal=c(1,0,1))                                #BIC=-657.73
Arima(holdLOG,order=c(3,1,2),seasonal=c(1,0,1))                                #BIC=-653.45
Arima(holdLOG,order=c(4,1,1),seasonal=c(1,0,1))                                #BIC=-653.06
Arima(holdLOG,order=c(2,1,1),seasonal=c(2,0,1))                                #BIC=-660
Arima(holdLOG,order=c(2,1,1),seasonal=c(2,0,2))                                #BIC=-654.94
Arima(holdLOG,order=c(2,1,1),seasonal=c(1,1,1))                                #BIC=-651.28
Arima(holdLOG,order=c(4,0,2),seasonal=c(0,1,1),include.drift = TRUE)           #BIC=-660.97
Arima(holdLOG,order=c(3,1,5),seasonal=c(3,0,1))                                #BIC=-663.16


############ SHORT LISTED MODELS
model1= Arima(holdLOG,order=c(2,1,1),seasonal=c(1,0,1))  
model2= Arima(holdLOG,order=c(3,1,5),seasonal=c(3,0,1))
acf(model1$residuals)
acf(model2$residuals)

############ Diagnostic checks using Box test
Box.test(model1$residuals,lag=20,type="Ljung-Box")
Box.test(model1$residuals,14,type="Ljung-Box")

Box.test(model2$residuals,lag=20,type="Ljung-Box")
Box.test(model2$residuals,17,type="Ljung-Box")

############ Diagnostic checks using Accuracy
accuracy(fore1,window(gasoline,start=c(1975,9)))
accuracy(fore2,window(gasoline,start=c(1975,9)))

############ Forecasts
fore1 = forecast(model1,h=4,lambda = 0,biasadj = TRUE)
fore2 = forecast(model2,h=4,lambda = 0,biasadj = TRUE)
fore2$mean

############ FORECASTING
upper=ts(fore2$upper[,2],start=c(1975,9),frequency = 12)
lower=ts(fore2$lower[,2],start=c(1975,9),frequency = 12)
plot(cbind(window(gasoline,start=c(1974,7)),fore2$mean,upper,lower),plot.type="single",lty=c("solid","solid","dotted","dotted"),col=c('blue','red','black','black'),ylab="Forecast")
sarima.for(holdLOG,n.ahead=24,3,1,5,3,0 ,1,12)
sarima.for(holdLOG,n.ahead=2,2,2,2,1,0 ,1,12)

############ ETS
modelETS=ets(hold)
foreETS=forecast(modelETS,h=4)
accuracy(foreETS,window(gasoline,start=c(1975,9)))

#### comparisaon of actual values and forecasted values
window(gasoline,start=c(1975,9))
fore2$mean
foreETS$mean

#### Re-estimation of the selected model using entire sample
gasolineLOG=log(gasoline)
model_with_entire_data = Arima(gasolineLOG,order=c(3,1,5),seasonal=c(3,0,1))
fore = forecast(model_with_entire_data,h=2,lambda = 0,biasadj = TRUE)
upper=ts(fore$upper[,2],start=c(1976,1),frequency = 12)
lower=ts(fore$lower[,2],start=c(1976,1),frequency = 12)
plot(cbind(window(gasoline,start=c(1974,7)),fore$mean,upper,lower),plot.type="single",lty=c("solid","solid","dotted","dotted"),col=c('blue','red','black','black'),ylab="Forecast")
sarima.for(gasolineLOG,n.ahead=24,3,1,5,3,0 ,1,12)
