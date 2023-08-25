#Econ 593 Mega HW2


##environment setting
install.packages("pacman")
library(pacman)

p_load(tidyverse,lmtest,tsibble, forecast,glmnet, fpp2,lmtest, cowplot,fontawesome,tsibbledata,gridExtra,xts,typlr,ggpubr)

set.seed(8)
state.name[max(round(49*runif(1)+1))]
########################################[a]#####################################
##data import and cleaning 
data<-read.csv("state_unem.csv",header = TRUE)
Mississippi <- data %>% filter(state_name == "Mississippi")
unemp_rate_miss <- ts(Mississippi$value, start = c(1980,1),end = c(2022,12),frequency = 12)
##plot
autoplot(unemp_rate_miss) +
  ggtitle("Monthly Unemployment rate (1980M1-2022M12)") + 
  xlab(" ") + ylab("Unemployment rate")

##1980:Paul Volcker's new monetary policy to raise policy to reduce economy activities with high unemployment rate
##2000: dot com bubble
##2008: financial crisis


########################################[b]#####################################
##linear regression model
unemp_ACF <- ggAcf(unemp_rate_miss, main="Unemployment rate Autocorrelation Plot")
unemp_PACF <- ggPacf(unemp_rate_miss, main="Unemployment rate Partial Autocorrelation Plot")

###to identify the stationary data by observe the ACF plot, weather the data drops to zero fast.
#obsequiously our Mississippi data non-stationary. to stabilize the mean, we can do a difference transformation

##log with first difference transformation
log.unemp_rate_miss <- ts(log(Mississippi$value), start = 1980,end = 2022,frequency = 12)
diff_log_unemp_ACF<- ggAcf(diff(log.unemp_rate_miss), main = "Log Unemployment rate Autocorrelation Plot", lag = 12)
diff_log_unemp_PACF<- ggPacf(diff(log.unemp_rate_miss), main = "Log Unemployment rate Partial Autocorrelation Plot", lag = 12)
##lagged by 12 to avoid seasonality
ACF_comparison <- ggarrange (unemp_ACF,
                        unemp_PACF,
                        diff_log_unemp_ACF,
                        diff_log_unemp_PACF
)
autoplot(log.unemp_rate_miss) +
  ggtitle("log Differenced Monthly Unemployment rate (1980M1-2022M12)") + 
  xlab(" ") + ylab("Unemployment rate")
print(ACF_comparison)

########################################[c]#####################################
train_data <- ts(Mississippi$value, start = 1980,end = c(2012,12),frequency = 12)

MODEL<- forecast::auto.arima(train_data)
MODEL

aic.tab <- matrix(NA, ncol = 3, nrow = 3)
bic.tab <- matrix(NA, ncol = 3, nrow = 3)
for(q in 0:2){
  for(p in 0:2){
    model <- Arima(train_data, order = c(p,0,q))
    aic.tab[q+1,p+1] <- AIC(model)
    bic.tab[q+1,p+1] <- BIC(model)
  }
}
colnames(aic.tab) <- c("p=0","p=1","p=2")
rownames(aic.tab) <- c("q=0","q=1","q=2")
colnames(bic.tab) <- c("p=0","p=1","p=2")
rownames(bic.tab) <- c("q=0","q=1","q=2")

aic.tab
bic.tab

#from the result of auto.arima and bic score, the best model for ARIMA is ARIMA(1,0,2)

ARIMA_102 <- Arima(train_data, order = c(1,0,2))
ARIMA_102

#evaluate residuals
checkresiduals(ARIMA_102, lag=12)

##as the P value close to 0, we reject the h0, and the data exist a AR process
#it might due to the MA(2) correlation




####diff train dataset
train_data <- ts(Mississippi$value, start = 1980,end = c(2012,12),frequency = 12)
diff_train_data <- diff(train_data)
MODEL_diff<- forecast::auto.arima(diff_train_data)
MODEL_diff

aic.tab <- matrix(NA, ncol = 6, nrow = 6)
bic.tab <- matrix(NA, ncol = 6, nrow = 6)
for(q in 0:5){
  for(p in 0:5){
    model <- Arima(diff_train_data, order = c(p,1,q))
    aic.tab[q+1,p+1] <- AIC(model)
    bic.tab[q+1,p+1] <- BIC(model)
  }
}
colnames(aic.tab) <- c("p=0","p=1","p=2","p=3","p=4","p=5")
rownames(aic.tab) <- c("q=0","q=1","q=2","q=3","q=4","q=5")
colnames(bic.tab) <- c("p=0","p=1","p=2","p=3","p=4","p=5")
rownames(bic.tab) <- c("q=0","q=1","q=2","q=3","q=4","q=5")

aic.tab
bic.tab

#ARIMA(5,1,3)
ARIMA_513 <- Arima(diff_train_data, order = c(5,1,3))
ARIMA_513

#evaluate residuals
checkresiduals(ARIMA_513, lag=12)




########################################[d]#####################################
##one step ahead recursive estimation h=1
test_data<- ts(Mississippi$value, frequency = 12, start = c(2013,1),end = c(2022,12))
Miss <-ts(Mississippi$value, start = c(1980,1), frequency = 12)
n.end <- 2013
pred_h1 <- matrix(NA, 120, 2)

# loop
for(i in 1:120){
  tmp0 <- 1980
  tmp1 <- n.end+(i-1)*1/12
  tmp <- window(Miss, tmp0, tmp1)
  pred_h1[i,1] <- window(Miss, tmp1, tmp1) # actual data
  trend <- seq_along(tmp)
  pred_h1[i,2] <- forecast(Arima(tmp, order = c(1,0,2)), h = 1)$mean #predicted data
}
# store results
forecast1 <- 
  ts(
    pred_h1,
    freq = 12, 
    start = c(2013,1), 
    names = c("Actual", "Trend Stationary")
  )

# plot
h1_forecast <- autoplot(forecast1,main = "Actual & Forecast unemployment rate by h=1")
h1_forecast
### COMPUT RMSE
rmse_h1 <- sqrt(mean((pred_h1[,1] - pred_h1[,2])^2))
rmse_h1 #[0.3567]




########################################[e]#####################################
n.end <- 2013.25
pred_h4 <- matrix(NA, 116, 2)
# loop
for(i in 1:116){
  tmp0 <- 1980
  tmp1 <- n.end+(i-1)*1/12
  tmp <- window(Miss, tmp0, tmp1)
  pred_h4[i,1] <- window(Miss, tmp1, tmp1) # actual data
  trend <- seq_along(tmp)
  pred_h4[i,2] <- forecast(Arima(tmp, order = c(1,0,2), xreg = trend), h = 4, xreg = length(tmp)+1)$mean #predicted data
}
# store results
forecast2 <- 
  ts(
    pred_h4,
    freq = 12, 
    start = c(2013,4), 
    names = c("Actual", "Trend Stationary")
  )

# plot
h4_forecast <- autoplot(forecast2,main = "Actual & ForecaSt unemployment rate by h=4")
h4_forecast

rmse_h4 <- sqrt(mean((pred_h4[,1] - pred_h4[,2])^2))
rmse_h4 #[0.3503013]



