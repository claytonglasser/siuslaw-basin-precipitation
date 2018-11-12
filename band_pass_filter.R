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
install.packages('geoR')
library(geoR)
install.packages('xts')
library(xts)
install.packages('rcompanion')
library(rcompanion)

#wrangle

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

siuslaw <- clean.csv %>% select(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, DATE, water_date, PRCP, SNOW) %>% group_by(NAME, water_date) %>% arrange(NAME) %>% filter(STATION == "US1ORLA0076" | STATION == "US1ORLA0003" | STATION == "US1ORLA0031" | STATION == "US1ORLA0091" | STATION == "USC00352973" | STATION == "USC00352972" | STATION == "USC00353995" | STATION == "US1ORLA0171" | STATION == "USC00355204" | STATION == "US1ORLA0132" | STATION == "USC00353995") %>% filter(water_date >= "2007-09-30" & water_date < "2017-10-01")

##creating inidivdual objects for distinct locations

US1ORLA0076 <- ungroup(siuslaw) %>% filter(STATION == "US1ORLA0076") %>% select(water_date,PRCP)

#normalize

##taking the log

US1ORLA0076$prcp_log <- ifelse(US1ORLA0076$PRCP > 0, log(na.omit(US1ORLA0076$PRCP)+1), 0)

head(US1ORLA0076)

ggplot(US1ORLA0076, aes(x=US1ORLA0076$water_date)) +
  geom_line(aes(y=US1ORLA0076$PRCP)) +
  geom_line(aes(y=US1ORLA0076$prcp_log), color="red")

#decompose

#test for stationarity

adf.test(US1ORLA0076$PRCP, alternative = "stationary")

##inspect for annual seasonality using band pass filter

US1ORLA0076_cffilter  <- cffilter(US1ORLA0076$prcp_log,pl=180,pu=365,root=FALSE,drift=FALSE,
                                  type=c("asymmetric"),
                                  nfix=NULL,theta=1)
View(US1ORLA0076_cffilter)

plot(US1ORLA0076_cffilter)
plot(US1ORLA0076$prcp_log)
lines(US1ORLA0076_cffilter$cycle , col="red")
lines(US1ORLA0076_cffilter$trend , col="blue")

##subtract trend and seasonal component from series

US1ORLA0076$decomp <- ((US1ORLA0076$prcp_log - US1ORLA0076_cffilter$trend) - US1ORLA0076_cffilter$cycle)

#---OR---#

cffilter_trend <- US1ORLA0076_cffilter$trend
cffilter_cycle <- US1ORLA0076_cffilter$cycle
cffilter_decomptmp <- US1ORLA0076$prcp_log - cffilter_trend
cffilter_decomp <- cffilter_decomptmp - cffilter_cycle
US1ORLA0076$decomp <- cffilter_decomp

##test remainder for normality

Acf(US1ORLA0076$decomp, main='',lag.max=1000)
Pacf(US1ORLA0076$decomp, main='',lag.max=1000) 

periodogram(US1ORLA0076$decomp, col = "black")
periodogram(US1ORLA0076$prcp_log, col = "black")

cpgram(US1ORLA0076$decomp) 
cpgram(US1ORLA0076$prcp_log) 

###converting all zeros to NA in order to plot only integers

US1ORLA0076_decomp_ints_only <- ifelse(cffilter_decomp == 0, NA, cffilter_decomp)
View(US1ORLA0076_decomp_ints_only)

plotNormalHistogram(US1ORLA0076_decomp_ints_only, prob = TRUE, col = "gray", main = "",
                    linecol = "blue", lwd = 2, length = 1000, breaks = 50)

#arima

US1ORLA0076_TS <- msts(US1ORLA0076$decomp, start=c(2007, 10), end=c(2018, 9),  seasonal.periods=365)

plot(US1ORLA0076_TS)

US1ORLA0076.fit <- auto.arima(US1ORLA0076_TS, seasonal=FALSE, xreg=fourier(US1ORLA0076_TS, K=1, h=365))
plot(forecast(US1ORLA0076.fit, h=365, xreg=fourier(US1ORLA0076_TS, K=1, h=365)))
tsdisplay(residuals(US1ORLA0076.fit), lag.max=1000, main='')
