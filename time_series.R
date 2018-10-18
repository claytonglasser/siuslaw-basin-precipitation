library(tidyverse)
library(xts)
library(ggplot2)
install.packages('forecast')
library(forecast)
install.packages('tseries')
library(tseries)
install.packages('imputeTS')
library(imputeTS)

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

siuslaw <- clean.csv %>% select(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, DATE, water_date, PRCP) %>% group_by(NAME, water_date) %>% arrange(NAME) %>% filter(STATION == "US1ORLA0076" | STATION == "US1ORLA0003" | STATION == "US1ORLA0031" | STATION == "US1ORLA0091" | STATION == "USC00352973" | STATION == "USC00352972" | STATION == "USC00353995" | STATION == "US1ORLA0171" | STATION == "USC00355204" | STATION == "US1ORLA0132" | STATION == "USC00353995") %>% filter(water_date >= "2007-10-01" & water_date <= "2017-09-30")

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

#creating time sereis for each location (STATION)

US1ORLA0076_TS <- ts(US1ORLA0076$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 12)
US1ORLA0003_TS <- ts(US1ORLA0003$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
US1ORLA0031_TS <- ts(US1ORLA0031$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
US1ORLA0091_TS <- ts(US1ORLA0091$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
USC00352973_TS <- ts(USC00352973$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
USC00352972_TS <- ts(USC00352972$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
US1ORLA0171_TS <- ts(US1ORLA0171$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
USC00355204_TS <- ts(USC00355204$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
US1ORLA0132_TS <- ts(US1ORLA0132$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)
USC00353995_TS <- ts(USC00353995$PRCP, start=c(2007, 10), end=c(2018, 9), frequency = 365)


#--------------------------------- Imputation must happen before forecasting

plotNA.distribution(US1ORLA0076_TS, main="NA's in US1ORLA0076")
plotNA.distribution(US1ORLA0003_TS, main="NA's in US1ORLA0003")
plotNA.distribution(US1ORLA0031_TS, main="NA's in US1ORLA0031")
plotNA.distribution(US1ORLA0091_TS, main="NA's in US1ORLA0091")
plotNA.distribution(USC00352973_TS, main="NA's in USC00352973")
plotNA.distribution(USC00352972_TS, main="NA's in USC00352972")
plotNA.distribution(US1ORLA0171_TS, main="NA's in US1ORLA0171")
plotNA.distribution(USC00355204_TS, main="NA's in USC00355204")
plotNA.distribution(US1ORLA0132_TS, main="NA's in US1ORLA0132")
plotNA.distribution(USC00353995_TS, main="NA's in USC00353995")

sum(is.na(US1ORLA0132_TS))

str(US1ORLA0132_TS)

US1ORLA0132_IMP_Struct <- na.kalman(US1ORLA0132_TS, model = "StructTS", smooth = TRUE)
US1ORLA0132_IMP_Arima <- na.kalman(US1ORLA0132_TS, model = "auto.arima", smooth = TRUE)

plotNA.imputations(US1ORLA0076_TS, US1ORLA0076_IMP, x.withTruth = NULL)


#--------------------------------- Forecasting

#seasonality and decomposition

US1ORLA0076_TS_decomp = stl(US1ORLA0076_TS, "periodic") #Error in na.fail.default(as.ts(x)) : missing values in object

US1ORLA0076_TS_deseasonal_cnt <- seasadj(US1ORLA0076_TS_decomp)

plot(US1ORLA0076_TS_decomp)

#test for staionarity vs trend

adf.test(US1ORLA0076_TS, alternative = "stationary")

#autocorrelation

acf(US1ORLA0076_TS, main='')

pacf(US1ORLA0076_TS, main='')

#seasonal differencing

US1ORLA0076_TS_count_dl = diff(US1ORLA0076_TS_deseasonal_cnt, differences = 1) #what is an appropriate score for the Dickey-Fuller value?nIt goes up in proportion to the number of differences.

plot(US1ORLA0076_TS_count_dl)

adf.test(US1ORLA0076_TS_count_dl, alternative = "stationary")

acf(US1ORLA0076_TS_count_dl, main='ACF for differenced series')

pacf(US1ORLA0076_TS_count_dl, main='PACF for differenced series') #The persistence of high values in acf plot probably represent a long term positive trend. 

#fitting ARIMA model

##get p d q values
auto.arima(US1ORLA0076_TS_deseasonal_cnt, seasonal = FALSE)
auto.arima(US1ORLA0076_TS_deseasonal_cnt, seasonal = TRUE)

##evaluate and iterate

US1ORLA0076_TS_fit <- auto.arima(US1ORLA0076_TS_deseasonal_cnt, seasonal = TRUE)
tsdisplay(residuals(US1ORLA0076_TS_fit), lag.max=50, main = '(0,0,0)') 

US1ORLA0076_TS_fit2 <- arima(US1ORLA0076_TS_deseasonal_cnt, order=c(2,0,0))
tsdisplay(residuals(US1ORLA0076_TS_fit2), lag.max=50, main = '(2,0,0)') 

par(mfrow=c(1,1))
US1ORLA0076_TS_fcast <- forecast(US1ORLA0076_TS_fit, h=12)
plot(US1ORLA0076_TS_fcast)

#test model performance with holdout set

US1ORLA0076_TS_holdout <-subset(ts(US1ORLA0076_TS_deseasonal_cnt), start = 2007, end = 2015)
US1ORLA0076_TS_no_holdout  = arima(subset(ts(US1ORLA0076_TS_deseasonal_cnt),  start = 2016, end = 2017), order=c(0,0,0)) 
fcast_US1ORLA0076_TS_no_holdout <- forecast(US1ORLA0076_TS_no_holdout, h=12)
plot(fcast_US1ORLA0076_TS_no_holdout, main='') 


lines(ts(US1ORLA0076_TS_deseasonal_cnt))

