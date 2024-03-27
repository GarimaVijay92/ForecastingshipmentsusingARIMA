library(forecast)
getwd()
setwd("C:/Users/vijay/OneDrive/Desktop/MSBA/2nd semester/Time series/Case study 2")

shipments <- read.csv("673_case2.csv")
head(shipments)

# creating time series data shipments.ts
shipments.ts <- ts(shipments$Shipments, 
                   start = c(2006, 1), end = c(2023, 4), freq = 4)
shipments.ts

#partitioning
nValid <- 20
nTrain <- length(shipments.ts) - nValid 
train.ts <- window(shipments.ts, start = c(2006, 1), end = c(2006, nTrain))
valid.ts <- window(shipments.ts, start = c(2006, nTrain + 1), 
                   end = c(2006, nTrain + nValid))
train.ts


#part a 
shipments.ar1<- Arima(shipments.ts, order = c(1,0,0))
summary(shipments.ar1)

#hypothesis testing
ar1 <- 0.7062
s.e. <- 0.0825
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}
#the results indicate that the data is not random walk and is predictable. 


#creating first difference with lag 1
diff.shipments <- diff(shipments.ts, lag = 1)
diff.shipments

#Creating plot for first differenced data. 
plot(diff.shipments, 
     xlab = "Time", ylab = "Shipments, $", xaxt = "n",
     ylim = c (-3500, 3000), main = "First Differencing of shipments", 
     bty = "l", lty = 5, lwd = 2, col="orange")
axis(1, at = seq(1, 250), labels = format(seq(1, 250)))


Acf(diff.shipments, lag.max = 8, 
    main = "Autocorrelation for Differenced Shipments", col = 'red')


#Regression model with linear trend and seasonality
train.lin.season <- tslm(train.ts ~ trend + season)
summary(train.lin.season)

train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)


#training model residuals 
train.lin.season.pred$residuals
#autocorrelation for training model residuals 
Acf(train.lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Shipments Training Residuals")


#AR(1) model for the regression residuals
res.ar1 <- Arima(train.lin.season$residuals, order = c(1,0,0))
summary(res.ar1)
res.ar1$fitted

Acf(res.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Shipments Training Residuals of Residuals")



#forecast for validation data 

train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred


res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean

valid.df <- round(data.frame(valid.ts, train.lin.season.pred$mean, 
                             res.ar1.pred$mean, valid.two.level.pred),3)
names(valid.df) <- c("Shipments", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df


#for entire dataset

lin.season <- tslm(shipments.ts ~ trend + season)
summary(lin.season)

lin.season.pred <- forecast(lin.season, h = 8, level = 0)
lin.season.pred

Acf(lin.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation of Regression Residuals for Entire Data Set")

residual.ar1 <- Arima(lin.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 8, level = 0)

summary(residual.ar1)

Acf(residual.ar1$residuals, lag.max = 8, 
    main = "Autocorrelation for Residuals of Residuals for Entire Data Set")

lin.season.ar1.pred <- lin.season.pred$mean + residual.ar1.pred$mean
lin.season.ar1.pred

table.df <- round(data.frame(lin.season.pred$mean, 
                             residual.ar1.pred$mean, lin.season.ar1.pred),3)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
table.df

#ARIMA model
train.arima.seas <- Arima(train.ts, order = c(1,1,1), 
                          seasonal = c(1,1,1)) 
summary(train.arima.seas)

train.arima.seas.pred <- forecast(train.arima.seas, h = nValid, level = 0)
train.arima.seas.pred

Acf(train.arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of ARIMA(1,1,1)(1,1,1) Model Residuals")


#Auto Arima model 

train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

Acf(train.auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


#comparing accuracy of both the models 
round(accuracy(train.arima.seas.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)


#seasonal arima and auto arima models for the entire dataset
arima.seas <- Arima(shipments.ts, order = c(1,1,1), 
                    seasonal = c(1,1,1)) 
summary(arima.seas)

arima.seas.pred <- forecast(arima.seas, h = 8, level = 0)
arima.seas.pred

Acf(arima.seas$residuals, lag.max = 8, 
    main = "Autocorrelations of Seasonal ARIMA (1,1,1)(1,1,1) Model Residuals")
#auto arima 

auto.arima <- auto.arima(shipments.ts)
summary(auto.arima)

auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred

Acf(auto.arima$residuals, lag.max = 8, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")

#Part e

round(accuracy(lin.season.pred$fitted, shipments.ts),3)
round(accuracy(lin.season$fitted + residual.ar1$fitted, shipments.ts), 3)
round(accuracy(arima.seas.pred$fitted, shipments.ts), 3)
round(accuracy(auto.arima.pred$fitted, shipments.ts), 3)
round(accuracy((snaive(shipments.ts))$fitted, shipments.ts), 3)
