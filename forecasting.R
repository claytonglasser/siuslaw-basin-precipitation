library(tidyverse)
library(ggplot2)
install.packages('forecast')
library(forecast)
install.packages('tseries')
library(tseries)
install.packages('TSA')
library(TSA)

#Wrangling the data

##Read in cleaned up csv as dataframe

clean.csv <- read_csv("siuslaw_basin_climate_clean.csv")

##Convert observations to needed object types

clean.csv$STATION <-as.factor(clean.csv$STATION)
clean.csv$NAME <-as.factor(clean.csv$NAME)
clean.csv$LATITUDE <-as.numeric(clean.csv$LATITUDE)
clean.csv$LONGITUDE <-as.numeric(clean.csv$LONGITUDE)
clean.csv$ELEVATION <-as.numeric(clean.csv$ELEVATION)
clean.csv$PRCP <-as.numeric(clean.csv$PRCP)
clean.csv$water_date <-as.Date(clean.csv$water_date)

##Subset DF down to only those stations with viable PRCP density within the selected timeframe

siuslaw <- clean.csv %>% select(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, DATE, water_date, PRCP, SNOW) %>% group_by(NAME, water_date) %>% arrange(NAME) %>% filter(STATION == "US1ORLA0076" | STATION == "US1ORLA0003" | STATION == "US1ORLA0031" | STATION == "US1ORLA0091" | STATION == "USC00352973" | STATION == "USC00352972" | STATION == "USC00353995" | STATION == "US1ORLA0171" | STATION == "USC00355204" | STATION == "US1ORLA0132" | STATION == "USC00353995") %>% filter(DATE >= "2007-10-01" & DATE < "2017-10-01")

head(siuslaw)

#creating inidivdual objects for distinct locations

US1ORLA0076 <- siuslaw %>% filter(STATION == "US1ORLA0076") 
US1ORLA0003 <- siuslaw %>% filter(STATION == "US1ORLA0003")
US1ORLA0031 <- siuslaw %>% filter(STATION == "US1ORLA0031")
US1ORLA0091 <- siuslaw %>% filter(STATION == "US1ORLA0091")
USC00352973 <- siuslaw %>% filter(STATION == "USC00352973")
USC00352972 <- siuslaw %>% filter(STATION == "USC00352972")
USC00353995 <- siuslaw %>% filter(STATION == "USC00353995")
US1ORLA0171 <- siuslaw %>% filter(STATION == "US1ORLA0171")
USC00355204 <- siuslaw %>% filter(STATION == "USC00355204")
US1ORLA0132 <- siuslaw %>% filter(STATION == "US1ORLA0132")
USC00353995 <- siuslaw %>% filter(STATION == "USC00353995")

#creating PRCP time series for each location (STATION)

US1ORLA0076_TS <- ts(US1ORLA0076$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365) 
US1ORLA0003_TS <- ts(US1ORLA0003$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0031_TS <- ts(US1ORLA0031$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0091_TS <- ts(US1ORLA0091$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00352973_TS <- ts(USC00352973$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00352972_TS <- ts(USC00352972$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0171_TS <- ts(US1ORLA0171$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00355204_TS <- ts(USC00355204$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0132_TS <- ts(US1ORLA0132$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00353995_TS <- ts(USC00353995$PRCP, start=c(2007, 10), end=c(2017, 9), frequency = 365)

#SNOW time series #not used

US1ORLA0076_TS_SNOW <- ts(US1ORLA0076$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365) 
US1ORLA0003_TS_SNOW <- ts(US1ORLA0003$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0031_TS_SNOW <- ts(US1ORLA0031$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0091_TS_SNOW <- ts(US1ORLA0091$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00352973_TS_SNOW <- ts(USC00352973$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00352972_TS_SNOW <- ts(USC00352972$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0171_TS_SNOW <- ts(US1ORLA0171$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00355204_TS_SNOW <- ts(USC00355204$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
US1ORLA0132_TS_SNOW <- ts(US1ORLA0132$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)
USC00353995_TS_SNOW <- ts(USC00353995$SNOW, start=c(2007, 10), end=c(2017, 9), frequency = 365)


sum(is.na(US1ORLA0076_TS))

#--------------------------------- Forecasting

#plot the series and visually examine it for any outliers, volatility, or irregularities. 

autoplot(US1ORLA0076_TS)

#clean up outliers and impute missing values

US1ORLA0076_TS_Clean = tsclean(US1ORLA0076_TS, replace.missing = TRUE)

sum(is.na(US1ORLA0076_TS_Clean))

autoplot(US1ORLA0076_TS_Clean)

#exploratory moving average (not using as data set but Ive seen it done)

US1ORLA0076_TS_ma = ma(US1ORLA0076_TS_Clean, order=30)
plot(US1ORLA0076_TS_Clean, type="l", col="black")
lines(US1ORLA0076_TS_ma,col="red",lwd=3)

#decompose and deseasonolize the data

US1ORLA0076_decomp = stl(US1ORLA0076_TS_Clean, s.window="periodic") #decompose te cleaned up data
plot(US1ORLA0076_decomp) #strong seasonal pattern, visiable in residuals

US1ORLA0076_deseasonal_cnt <- seasadj(US1ORLA0076_decomp) #deseasonalize the decomposed data
plot(US1ORLA0076_deseasonal_cnt) #this is what is fed into the ARIMA

#test for stationarity

adf.test(US1ORLA0076_TS_Clean, alternative = "stationary") # Dickey-Fuller = -9.9361, Lag order = 15, p-value = 0.01 indicates data is stationary. Thus auto.arima does not suggest differencing.

#Autocorrelations and Choosing Model Order

#testing the clean time series
Acf(US1ORLA0076_TS_Clean, main='')  #big first lag, sine wave pattern
Pacf(US1ORLA0076_TS_Clean, main='') #cuts off after first few lags

#Add differincing

US1ORLA0076_TS_d1 = diff(US1ORLA0076_deseasonal_cnt, differences = 1)

plot(US1ORLA0076_TS_d1) #differenced but did it really chnage anything? Overdifferenced?
adf.test(US1ORLA0076_TS_d1, alternative = "stationary") #Dickey-Fuller = -24.593, Lag order = 15, p-value = 0.01

Acf(US1ORLA0076_TS_d1, main='') #ery large spike at lags 1 and 2 and no other significant spikes, indicating that in the absence of differencing an AR(3) model should be used or an AR(3) with differencing

Pacf(US1ORLA0076_TS_d1, main='') #similar structure but with more spikes; might beneftit from some MA terns

#Fitting an ARIMA model

auto.arima(US1ORLA0076_deseasonal_cnt, seasonal=FALSE) 
#------------------------------------------------------
#ARIMA(3,0,1) with non-zero mean 

#Coefficients:
#  ar1      ar2      ar3      ma1    mean
#1.3409  -0.3271  -0.0288  -0.9277  0.1251
#s.e.  0.0236   0.0276   0.0182   0.0166  0.0121

#sigma^2 estimated as 0.02349:  log likelihood=1668.77
#AIC=-3325.54   AICc=-3325.51   BIC=-3288.32
#------------------------------------------------------

#Evaluate and Iterate

US1ORLA0076_fit<-auto.arima(US1ORLA0076_deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(US1ORLA0076_fit), lag.max=750, main='auto.arima 3,0,1') #these residuals don't quite seem lik white noise, and there's alot of spikes, but this is the best one I found

US1ORLA0076_fit2 = arima(US1ORLA0076_deseasonal_cnt, order=c(2,0,1))
tsdisplay(residuals(US1ORLA0076_fit2), lag.max=750, main='ARIMA 2,0,1')

US1ORLA0076_fit3 = arima(US1ORLA0076_deseasonal_cnt, order=c(1,0,1))
tsdisplay(residuals(US1ORLA0076_fit3), lag.max=750, main='1,0,1')


#forcasting

US1ORLA0076_fcast <- forecast(US1ORLA0076_fit, h=365) #using auto.arima suggestion
plot(US1ORLA0076_fcast) #forecast sucks; very naive

#hold out set
US1ORLA0076_predictthis <- window(ts(US1ORLA0076_deseasonal_cnt), start=3001)
plot(US1ORLA0076_predictthis)

#leave in set
US1ORLA0076_fit_leavethis = arima(window(ts(US1ORLA0076_deseasonal_cnt), start=1, end=3000), order=c(3,0,1))
plot(window(ts(US1ORLA0076_deseasonal_cnt), start=1, end=3000))

#forecast
US1ORLA0076_fcast_predictthis <- forecast(US1ORLA0076_fit_leavethis,h=365)
plot(US1ORLA0076_fcast_predictthis, col="black")
lines(ts(US1ORLA0076_predictthis),col="red") #plots at the front of graph instead of chronologically

#Evaluate and Iterate

##adding seasnality back in
US1ORLA0076_fit_w_seasonality = auto.arima(US1ORLA0076_deseasonal_cnt, seasonal=TRUE)

#forecast
US1ORLA0076_seas_fcast <- forecast(US1ORLA0076_fit_w_seasonality, h=365)
plot(US1ORLA0076_seas_fcast) #forecast still sucks

#using ARIMAX to add SNOW data: not enough snowfall to make this worth it