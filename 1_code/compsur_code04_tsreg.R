# ------------
# Introduction
# ------------

## Runs timeseries regression to understand if food prices really drive gender gap

rm(list=ls())
NAME <- 'code04_tsreg' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

library('tidyverse')
library(tseries) # for timeseries test
library(forecast)

## --------
## Settings
## --------
### Any settings go here

f <- 'compsur'


## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline',f))){
  pipeline <- file.path('empirical', '2_pipeline',f, NAME)
} else {
  pipeline <- file.path('2_pipeline',f, NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}


### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  outline <- file.path('empirical', '3_output','results',f,NAME)
  dir.create(outline)
}



# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

D <- read_csv(file.path('empirical', '2_pipeline', 'compsur','code02_tsgendergap','out', 'D.csv')) %>%
  mutate(date = as.Date(date))

## -- Make time series data

us_tot_mich <- ts(data = D$cpi_tot_us[D$date >= as.Date('1978-01-01')], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)

us_food_mich <- ts(data = D$cpi_food_us[D$date >= as.Date('1978-01-01')], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)

us_tot_sce <- ts(data = D$cpi_tot_us[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                  start=c(2013, 06), 
                  end=c(2020, 11), 
                  frequency = 12)

us_food_sce <- ts(data = D$cpi_food_us[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                   start=c(2013, 06), 
                   end=c(2020, 11), 
                   frequency = 12)

germany_tot <- ts(data = D$cpi_tot_germany[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)

germany_food <- ts(data = D$cpi_food_germany[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)

euro_tot <- ts(data = D$cpi_tot_euro[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2004, 01), 
             end=c(2022, 09), 
             frequency = 12)

euro_food <- ts(data = D$cpi_food_euro[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2004, 01), 
             end=c(2022, 09), 
             frequency = 12)

msc <- ts(data = D$Michigan[D$date >= as.Date('1978-01-01')], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)

sce <- ts(data = D$FRBNY[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
             start=c(2013, 06), 
             end=c(2020, 11), 
             frequency = 12)

bop <- ts(data = D$BOP[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)

ecfin <- ts(data = D$ECFIN[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2004, 01), 
             end=c(2022, 09), 
             frequency = 12)


Tmsc <- ts.union(us_food_mich,us_tot_mich,msc)
Tsce <- ts.union(us_food_sce,us_tot_sce,sce)
Tbop <- ts.union(germany_food,germany_tot,bop)
Teuro <- ts.union(euro_food,euro_tot,ecfin)


# ## -- Structural Decomposition
# 
# 
# bop_dec <- decompose(na.StructTS(bop))
# plot(bop_dec)
# bop_deseason <- bop - bop_dec$seasonal
# 
# ecfin_dec <- decompose(na.StructTS(ecfin))
# plot(ecfin_dec)
# ecfin_deseason <- ecfin - ecfin_dec$seasonal
# 
# msc_dec <- decompose(na.StructTS(msc))
# plot(msc_dec)
# msc_deseason <- msc - msc_dec$seasonal
# msc_detrended <- msc_deseason/msc_dec$trend
# 
# sce_dec <- decompose(na.StructTS(sce))
# plot(sce_dec)
# sce_deseason <- sce - sce_dec$seasonal
# 
# par(mfrow = c(1,2))
# plot.ts(msc_dec$random, main="Residuals", ylab="")
# hist(msc_dec$random, breaks = 25, freq = F, main = "Histogram")
# 
# 
# ## -- Autocorrelation and ARIMA
# 
# # Correlelogram
# 
# acf(msc_deseason)
# pacf(msc_deseason)
# 
# # Cross-Correlation
# 
# # fit an ARIMA model
# x_model <- auto.arima(us_food_mich)
# # keep the residuals ("white noise")
# x_residuals <- x_model$residuals
# 
# # fit the same ARIMA model to the Y-series
# # by using the "Arima" function in forecast
# y_model <- Arima(msc, model=x_model)
# # keep the residuals
# y_filtered <- residuals(y_model)
# 
# # apply the ccf to the residuals
# ccf(x_residuals, y_filtered)
# 
# ## -- Test Stationarity
# 
# # KPSS Test
# kpss.test(msc, null = "Trend")
# kpss.test(msc, null = "Level")
# # results in stochastic trend but the differenced series being trend stationary
# 
# # ADF Test
# adf.test(diff(msc))
# # cannot reject null of unit root

## -- Run static time series

Tbop <- na.remove(Tbop)
bop <- lm(bop ~ germany_food + germany_tot + lag(germany_food) + lag(germany_tot), data = Tbop, na.action=na.exclude)
summary(bop)

Teuro <- na.remove(Teuro)
ecfin <- lm(ecfin ~ euro_food + euro_tot + lag(euro_food) + lag(euro_tot), data = Teuro, na.action=na.exclude)
summary(ecfin)

Tmsc <- na.remove(Tmsc)
msc <- lm(msc ~ us_food_mich + us_tot_mich + lag(us_food_mich) + lag(us_tot_mich), data = Tmsc, na.action=na.exclude)
summary(msc)

Tsce <- na.remove(Tsce)
sce <- lm(sce ~ us_food_sce + us_tot_sce + lag(us_food_sce) + lag(us_tot_sce), data = Tsce, na.action=na.exclude)
summary(sce)

# --- Print output

# settings for stargazer
column.labels <- c("MSC","SCE","ECFIN","BOP")
title <- "Comparing estimators"
label <- "tab:timeseries"
dep.var.labels <- "Gender gap in inflation expectation, 12 months ahead, point estimate"


writeLines(capture.output(stargazer(msc, sce, ecfin, bop, 
                                    title = title, label = label, 
                                    column.labels = column.labels,  model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline, 'code_tsreg.tex'))


