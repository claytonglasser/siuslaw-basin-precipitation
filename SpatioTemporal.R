#List of potentially helpful packages

library(tidyverse)
library(lubridate)
library(naniar)
library(ggmap)
library(mapproj)
library(plyr)
library(SpatioTemporal)
library(Matrix)
library(plotrix)
library(maps)

#Wrangling the data

##Read in cleaned up csv as dataframe

clean.csv <- read_csv("siuslaw_basin_climate_clean.csv")
str(clean.csv)
head(clean.csv)

##Convert observations to needed object types

clean.csv$STATION <-as.factor(clean.csv$STATION)
clean.csv$NAME <-as.factor(clean.csv$NAME)
clean.csv$LATITUDE <-as.numeric(clean.csv$LATITUDE)
clean.csv$LONGITUDE <-as.numeric(clean.csv$LONGITUDE)
clean.csv$ELEVATION <-as.numeric(clean.csv$ELEVATION)
clean.csv$PRCP <-as.numeric(clean.csv$PRCP)
clean.csv$SNOW <-as.numeric(clean.csv$SNOW)
clean.csv$calendar_year <-as.factor(clean.csv$calendar_year)
clean.csv$water_year <-as.factor(clean.csv$calendar_year)
clean.csv$water_date <-as.Date(clean.csv$water_date)
clean.csv$TMIN <-as.numeric(clean.csv$TMIN)
clean.csv$TMAX <-as.numeric(clean.csv$TMAX)
clean.csv$TAVG <-as.numeric(clean.csv$TAVG)

##Subset DF down to only those stations with viable PRCP density within the selected timeframe

siuslaw <- clean.csv %>% select(STATION, NAME, LATITUDE, LONGITUDE, ELEVATION, DATE, calendar_year, month, day, water_year, water_date, PRCP, SNOW, SNWD, TAVG, TMAX, TMIN, TOBS) %>% group_by(NAME, water_year) %>% arrange(NAME) %>% filter(STATION == "US1ORLA0076" | STATION == "US1ORLA0003" | STATION == "US1ORLA0031" | STATION == "US1ORLA0091" | STATION == "USC00352973" | STATION == "USC00352972" | STATION == "USC00353995" | STATION == "US1ORLA0171" | STATION == "USC00355204" | STATION == "US1ORLA0132" | STATION == "USC00353995") %>% filter(water_date >= "2007-10-01" & water_date <= "2017-09-30")

#Creaitng the two dataframes required for createSTdata

##obs
siuslaw.obs <- ungroup(siuslaw) %>% select(water_date, STATION, PRCP)
colnames(siuslaw.obs) <- c("date", "ID", "obs") #column names required exactly as is

head(siuslaw.obs)

###siuslaw.obs.wide <- siuslaw.obs %>% spread(STATION, PRCP, fill = NA, convert = FALSE) #was used for matrix; now using DF format instead

##covars
siuslaw.covars.tmp <- ungroup(siuslaw) %>% select(STATION, LATITUDE, LONGITUDE, ELEVATION) #convenience variable for getting the geo data down to unique instances
siuslaw.covars <- unique(siuslaw.covars.tmp) 
colnames(siuslaw.covars) <- c("ID", "LATITUDE", "LONGITUDE", "ELEVATION") #STdata DFs match up on ID column

siuslaw.covars

#create STdata object

siuslaw.ST <- createSTdata(obs=siuslaw.obs, covars=siuslaw.covars) #SpationTemporal DF/matrix not indluded as this process does not require additonal covariates (or does ELEVATION need to be in there?)

#Plot occurance of observations

layout(matrix(c(1,2,1,3), 2, 2))
par(mar=c(2.3,3.3,2,1), mgp=c(2,1,0))
plot(siuslaw.ST, "loc", main="Occurrence of Observations", xlab="", ylab = "Location", col=c("black", "red"), legend.loc=NULL)

#QQ Plot

par(mar=c(3.3,3.3,2,1))
qqnorm(siuslaw.ST, line=1) #sample data is apparently right-skewed. Is that a problem? Not gaussian enough for imputation assumptions?

#Plot PRCP against ELEVATION grouped by location; not that useful?

scatterPlot(siuslaw.ST, covar="ELEVATION", xlab="ELEVATION", ylab="PRCP", pch=19, cex=.25, smooth.args=list(span=4/5,degree=2))

#Temporal Basis Functions

Siuslaw.Matrix <- createDataMatrix(siuslaw.ST)

summary(Siuslaw.Matrix)

#Determining the Number of Basis Functions

SVD.cv <- SVDsmoothCV(Siuslaw.Matrix, 0:4)

print(SVD.cv)
plot(SVD.cv) ##All four statistics flatten out noticable after 2 basis functions, indicating that 2 basis functions is likely to provide the most efficient description of the temporal variability

##We now use the updateTrend function to add 2 smooth temporal basis functions to the STdata-object

siuslaw.ST <- updateTrend(siuslaw.ST, n.basis=2)

##Alternatively calcSmoothTrends can be used to compute both the basis functions based on all data and those obtained when excluding each column in the data-matrix

smooth.trend <- calcSmoothTrends(siuslaw.ST, n.basis=2, cv=TRUE) ##This allows for a sensitivity analysis of the temporal basis functions.

#This allows for a sensitivity analysis of the temporal basis functions. Here we illustrate this by the fit at one location, but the different temporal basis functions could be carried through the entire analysis.

siuslaw.ST.cv <- vector("list", length(smooth.trend$trend.fnc.cv))

for(i in 1:length(siuslaw.ST.cv)){
   suppressMessages(siuslaw.ST.cv[[i]] <- updateTrend(siuslaw.ST,
                                                       fnc=smooth.trend$trend.fnc.cv[[i]]))
   }

plot(siuslaw.ST, main="Possible temporal trends",
        xlab="TIME", ylab="PRCP", pch=c(19,NA), cex=.25)
for(i in 1:length(siuslaw.ST.cv)){
   plot(siuslaw.ST.cv[[i]], add=TRUE, col=i, pch=NA, lty=c(NA,2))
} 

#Evaluating the Basis Functions

##Temporal Basis Functions

### TThe plot.STdata function can now be used to evaluate how well the temporal basis functions capture the temporal structure. plot.STdata fits a linear regression of observations for a particular location of the smooth basis functions, and plots fitted values, residuals, auto-correlation, or partial auto-correlation functions. Here the two temporal basis func tions capture the temporal variability in the data, as illustrated by residuals and correlation functions.

unique(siuslaw$STATION)

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(siuslaw.ST, "obs", ID="US1ORLA0076",
       xlab="", ylab="PRCP",
       main="Temporal trend US1ORLA0076")
plot(siuslaw.ST, "res", ID="US1ORLA0076",
       xlab="", ylab="PRCP")
plot(siuslaw.ST, "acf", ID="US1ORLA0076")
plot(siuslaw.ST, "pacf", ID="US1ORLA0076")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(siuslaw.ST, "obs", ID="US1ORLA0003",
     xlab="", ylab="PRCP",
     main="Temporal trend US1ORLA0003")
plot(siuslaw.ST, "res", ID="US1ORLA0003",
     xlab="", ylab="PRCP")
plot(siuslaw.ST, "acf", ID="US1ORLA0003")
plot(siuslaw.ST, "pacf", ID="US1ORLA0003")

par(mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0))
layout(matrix(c(1,1,2,2,3,4), 3, 2, byrow=TRUE))
plot(siuslaw.ST, "obs", ID="US1ORLA0031",
     xlab="", ylab="PRCP",
     main="Temporal trend US1ORLA0031")
plot(siuslaw.ST, "res", ID="US1ORLA0031",
     xlab="", ylab="PRCP")
plot(siuslaw.ST, "acf", ID="US1ORLA0031")
plot(siuslaw.ST, "pacf", ID="US1ORLA0031")

#Deterministic Basis Functions

## In lieu of the SVD based basis functions we could use a set of deterministic temporal functions, e.g. f1(t) = 1, f2(t) = 2πt/365, f3(t) = sin(2πt/365), and f4(t) = cos(2πt/365). Specifying these and comparing to the data driven smooths extrated above we note that, for this data, three deterministic basis functions achieve a slightly worse fit than the two functions based on smoothed SVDs.

### I Need to make my own evaluation of this comparison

siuslaw.ST.fnc <- updateTrend(siuslaw.ST, fnc=function(x){
  x = 2*pi*as.numeric(x)/365;
  return( cbind(x, sin(x), cos(x)) )})
par(mfrow=c(2,1), mar=c(2.3,3.3,1.5,1), mgp=c(2,1,0))
for(i in c("US1ORLA0076","US1ORLA0003")){
  plot(siuslaw.ST, ID=i, pch=c(19,NA), cex=.25, xlab="",
         ylab="siuslaw.ST", main=paste("Station",i))
  plot(siuslaw.ST.fnc, ID=i, add=TRUE, col=2, pch=NA)
  }

#Specifying the Model

##Helpful packages
install.packages("geoR")
library(geoR)
install.packages("fields")
library(fields)
install.packages("gplots")
library(gplots)

#Suitable covariates and covariance structures for the β-fields can be determined by considering the empirical estimates of these fields obtained by regressing the observations at each location on the temporal basis functions. The resulting regression coefficients can be analysed using standard geo-statistical software, e.g. provided by the R-packages geoR (Ribeiro and Diggle 2001) or fields (Furrer et al. 2012), to determine suitable mean and covariance models (see also Mercer et al. 2011). Here we briefly illustrate the point by computing the regression coefficents and comparing them to a few possible covariates

### I am only using ELEVATION as  covariate and may forgo that too, so this may not be relevant. 

##plot regression coefficients of ELEVATION

beta.lm <- estimateBetaFields(siuslaw.ST)
par(mfrow=c(1,2), mar=c(3.3,2.3,1.5,1), mgp=c(2,1,0))
plotCI(siuslaw.ST$covars$ELEVATION, beta.lm$beta[,1],
         uiw=1.96*beta.lm$beta.sd[,1], ylab="", xlab="Elevation",
         main="Beta-field for Elevation")

#Specifying the model

LUR <- list(~ELEVATION) #I don't think this is constructed correctly
cov.beta <- list(covf="exp", nugget=FALSE) #I don't know what NUGGET is, or what you can set it to. Tutorial has it set to "type" which I do not need
cov.nu <- list(covf="exp", nugget=~ELEVATION, random.effect=FALSE)

locations <- list(coords=c("LONGITUDE","LATITUDE"), long.lat=c("LONGITUDE","LATITUDE")) #not sure if I can exclude one of these arguments

siuslaw.ST.model <- createSTmodel(siuslaw.ST, LUR=LUR, 
                                  ST=NULL,
                              cov.beta=cov.beta, cov.nu=cov.nu,
                              locations=locations)

siuslaw.ST$covars

list(siuslaw.ST$ELEVATION)

#----
#NOTES

#Can't get LUR=LUR to work, becuase of ELEVATION issues

#Since ELEVATION only varies with location, shouldn't it's corrlation be identical? i.e. not add any info? Perhaps not neccsary to include.

#----

#Estimation

###To avoid potential numerical optimisation issues, the estimation function allows for multiple starting points, returning all optima found. The functions loglikeSTdim and loglikeSTnames gives the number of parameters (and other model dimension) and the names, i.e. expected order, of the parameters. Using this information a two column matrix, where each column represents a different optimisation starting point, is constructed:

dim <- loglikeSTdim(siuslaw.ST.model)
x.init <- cbind(c( rep(2, dim$nparam.cov-1), 0),
                  c( rep(c(1,-3), dim$m+1), -3, 0))
rownames(x.init) <- loglikeSTnames(siuslaw.ST.model, all=FALSE)

##Model parameters are then estimated through

est.siuslaw.ST.model <- estimate(siuslaw.ST.model, x.init, type="p", hessian.all=TRUE) ###This won't finish running, so this variable doesn't exist for later use: "Bad direction in the line search; refresh the lbfgs memory and restart the iteration."

#Prediction ##Won't work without fixing estimation

pred <- predict(siuslaw.ST.model, est.siuslaw.ST.model, LTA=TRUE, type="p")
pred.log <- predict(siuslaw.ST.model, est.siuslaw.ST.model, LTA=TRUE,
                      transform="unbiased", type="p")

par(mfrow=c(2,2), mar=c(3.3,3.3,1.5,1), mgp=c(2,1,0), pty="s")
for(i in 1:3){
  plotCI(x=beta.lm$beta[,i], y=pred$beta$EX[,i],
           uiw=1.96*beta.lm$beta.sd[,i], err="x",
           pch=NA, sfrac=0.005,
           main=paste("Beta-field for f", i, "(t)", sep=""),
           xlab="Empirical estimate",
           ylab="Spatio-Temporal Model")
  plotCI(x=beta.lm$beta[,i], y=pred$beta$EX[,i],
           uiw=1.96*sqrt(pred$beta$VX[,i]),
           add=TRUE, pch=NA, sfrac=0.005)
  abline(0, 1, col="grey")
  }
