library(tidyverse)
library(ggplot2)
install.packages('forecast')
library(forecast)
install.packages('tseries')
library(tseries)
install.packages('TSA')
library(TSA)
install.packages('zoo')
library(zoo)
install.packages('prophet')
library(prophet)
install.packages('stlplus')
library(stlplus)
install.packages('mFilter')
library(mFilter)
install.packages('seas')
library(seas)
install.packages('deseasonalize')
library(deseasonalize)
install.packages('MAPA')
library(MAPA)

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

write.csv(US1ORLA0076, file = "US1ORLA0076.csv")

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

#msts

US1ORLA0076_TS <- msts(US1ORLA0076$PRCP, start=c(2008, 10), end=c(2017, 9),  seasonal.periods=c(365.25))


plot(US1ORLA0076_TS)

US1ORLA0076_TS_d2 = diff(US1ORLA0076_TS, differences = 2)

US1ORLA0076_decomp = stl(US1ORLA0076_TS, s.window = "periodic") #decompose the cleaned up data
plot(US1ORLA0076_decomp) #strong seasonal pattern, visiable in residuals

US1ORLA0076_StructTS = StructTS(US1ORLA0076_TS, type = "level", init = NULL,
         fixed = NULL, optim.control = NULL)
plot(US1ORLA0076_StructTS)

US1ORLA0076_deseasonal_cnt <- seasadj(US1ORLA0076_decomp) #deseasonalize the decomposed data
plot(US1ORLA0076_deseasonal_cnt) #this is what is fed into the ARIMA

adf.test(US1ORLA0076_deseasonal_cnt, alternative = "stationary") #is stationary


Acf(US1ORLA0076_deseasonal_cnt, main='',lag.max=1000)

Pacf(US1ORLA0076_deseasonal_cnt, main='',lag.max=1000)


US1ORLA0076_TS.fit <- auto.arima(US1ORLA0076_TS, seasonal=FALSE, xreg=fourier(US1ORLA0076_TS, K=5))
plot(forecast(US1ORLA0076_TS.fit, h=365, xreg=fourier(US1ORLA0076_TS, K=5, h=365)))
tsdisplay(residuals(US1ORLA0076_TS.fit), lag.max=1000, main='')


m <- prophet(US1ORLA0076_TS)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)

#--------------stlplus

US1ORLA0076_decompplus <- stlplus(US1ORLA0076_TS, n.p = 365.25, s.window = "periodic", outer = 6)
plot(US1ORLA0076_decompplus) #this looks like a better decomp; does more outer robustness iterations improve it?


#--------------mfilter

US1ORLA0076_mfilter <- bkfilter(US1ORLA0076_TS)
plot(US1ORLA0076_mfilter)

US1ORLA0076_cffilter  <- cffilter(US1ORLA0076_TS)
plot(US1ORLA0076_cffilter)

US1ORLA0076_hpfilter  <- hpfilter(US1ORLA0076_TS)
plot(US1ORLA0076_hpfilter)

#---------------seas

US1ORLA0076_tmp_seas <- clean.csv %>% select(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, DATE, water_date, PRCP, SNOW) %>% group_by(NAME, water_date) %>% arrange(NAME) %>% filter(STATION == "US1ORLA0076") %>% filter(DATE >= "2007-10-01" & DATE < "2017-10-01")

US1ORLA0076_seas <- ungroup(as.data.frame(US1ORLA0076_tmp_seas)) %>% select(water_date, PRCP) 

US1ORLA0076_seas <- rename(US1ORLA0076_seas,"date" = "water_date")

US1ORLA0076_seassum <- seas.sum(US1ORLA0076_seas, "PRCP", width = 7, start.day = 1, "PRCP",
         a.cut = 0.3, na.cut = 0.2)

plot(US1ORLA0076_seassum)

US1ORLA0076_seasnorm <- seas.norm(US1ORLA0076_seassum)
plot(US1ORLA0076_seasnorm)

#------------deseasonalize
 
US1ORLA0076_ds <- ds(US1ORLA0076_TS, Fm = 1, Fs = 6, type = "daily", searchQ=TRUE, lag.max=20, ic="BIC", standardizeQ  = FALSE)
summary(US1ORLA0076_ds)
cpgram(US1ORLA0076_ds$z) #meaning?
plot(US1ORLA0076_ds$z) #wtf?

#------------MAPA

mapa(US1ORLA0076_TS, 365.25, fh=365, ifh=1, minimumAL=1, maximumAL=5,
     comb="w.mean", paral=0,
     display=1, outplot=1, hybrid=TRUE, model="ZZZ",
     type="es", conf.lvl=NULL)

