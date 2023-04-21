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
library(stargazer)
library(data.table)

## --------
## Settings
## --------
### Any settings go here

f <- 'compsur'

mo <- 'mean'
# can go mean, sd, 25, 50, 75


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


if (dir.exists(file.path('empirical', '3_output','results',f,NAME))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  dir.create(outline)
}



# ---------
# Functions
# ---------




# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

D <- read_csv(file.path('empirical', '2_pipeline', 'compsur','code03_addquali','out', 'D.csv')) %>%
  mutate(date = as.Date(date))

## -- Make time series data

# Inflation data

us_tot_mich <- ts(data = D$cpi_tot_us[D$date >= as.Date('1978-01-01')], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)
us_tot_mich_lag <- shift(us_tot_mich, n=1, fill=NA, type="lag")
us_tot_mich_cv <- ts(data = D$cpi_tot_us_cv[D$date >= as.Date('1978-01-01')], 
                  start=c(1978, 01), 
                  end=c(2022, 12), 
                  frequency = 12)


us_food_mich <- ts(data = D$cpi_food_us[D$date >= as.Date('1978-01-01')], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)
us_food_mich_lag <- shift(us_food_mich, n=1, fill=NA, type="lag")
us_food_mich_cv <- ts(data = D$cpi_food_us_cv[D$date >= as.Date('1978-01-01')], 
                   start=c(1978, 01), 
                   end=c(2022, 12), 
                   frequency = 12)


us_tot_sce <- ts(data = D$cpi_tot_us[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                  start=c(2013, 06), 
                  end=c(2020, 11), 
                  frequency = 12)
us_tot_sce_lag <- shift(us_tot_sce, n=1, fill=NA, type="lag")
us_tot_sce_cv <- ts(data = D$cpi_tot_us_cv[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                 start=c(2013, 06), 
                 end=c(2020, 11), 
                 frequency = 12)

us_food_sce <- ts(data = D$cpi_food_us[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                   start=c(2013, 06), 
                   end=c(2020, 11), 
                   frequency = 12)
us_food_sce_lag <- shift(us_food_sce, n=1, fill=NA, type="lag")
us_food_sce_cv <- ts(data = D$cpi_food_us_cv[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01')], 
                  start=c(2013, 06), 
                  end=c(2020, 11), 
                  frequency = 12)


germany_tot <- ts(data = D$cpi_tot_germany[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)
germany_tot_lag <- shift(germany_tot, n=1, fill=NA, type="lag")
germany_tot_cv <- ts(data = D$cpi_tot_germany_cv[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
                  start=c(2019, 05), 
                  end=c(2022, 09), 
                  frequency = 12)

germany_food <- ts(data = D$cpi_food_germany[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)
germany_food_lag <- shift(germany_food, n=1, fill=NA, type="lag")
germany_food_cv <- ts(data = D$cpi_food_germany_cv[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01')], 
                   start=c(2019, 05), 
                   end=c(2022, 09), 
                   frequency = 12)

euro_tot <- ts(data = D$cpi_tot_euro[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2004, 01), 
             end=c(2022, 09), 
             frequency = 12)
euro_tot_lag <- shift(euro_tot, n=1, fill=NA, type="lag")
euro_tot_cv <- ts(data = D$cpi_tot_euro_cv[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
               start=c(2004, 01), 
               end=c(2022, 09), 
               frequency = 12)

euro_food <- ts(data = D$cpi_food_euro[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
             start=c(2004, 01), 
             end=c(2022, 09), 
             frequency = 12)
euro_food_lag <- shift(euro_food, n=1, fill=NA, type="lag")
euro_food_cv <- ts(data = D$cpi_food_euro_cv[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01')], 
                start=c(2004, 01), 
                end=c(2022, 09), 
                frequency = 12)

# add quantitative expectations

msc <- ts(data = D[D$date >= as.Date('1978-01-01'),paste('msc_',mo, sep = "")], 
             start=c(1978, 01), 
             end=c(2022, 12), 
             frequency = 12)

sce <- ts(data = D[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01'),paste('sce_',mo, sep = "")], 
             start=c(2013, 06), 
             end=c(2020, 11), 
             frequency = 12)

bop <- ts(data = D[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01'),paste('bop_',mo, sep = "")], 
             start=c(2019, 05), 
             end=c(2022, 09), 
             frequency = 12)

if(mo!="sd"){
  ecfin <- ts(data = D[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01'),paste('ecfin_',mo, sep = "")], 
               start=c(2004, 01), 
               end=c(2022, 09), 
               frequency = 12)
}

# add qualitative data

qmsc <- ts(data = D[D$date >= as.Date('1978-01-01'),'qmsc'], 
          start=c(1978, 01), 
          end=c(2022, 12), 
          frequency = 12)

qsce <- ts(data = D[D$date >= as.Date('2013-06-01') & D$date <= as.Date('2020-11-01'),'qsce'], 
          start=c(2013, 06), 
          end=c(2020, 11), 
          frequency = 12)

qbop <- ts(data = D[D$date >= as.Date('2019-05-01') & D$date <= as.Date('2022-09-01'),'qbop'], 
          start=c(2019, 05), 
          end=c(2022, 09), 
          frequency = 12)
if(mo!="sd"){
  qecfin <- ts(data = D[D$date >= as.Date('2004-01-01') & D$date <= as.Date('2022-09-01'),'qecfin'], 
              start=c(2004, 01), 
              end=c(2022, 09), 
              frequency = 12)
}


Tmsc <- ts.union(us_food_mich,us_tot_mich,us_food_mich_lag,us_tot_mich_lag,us_food_mich_cv,us_tot_mich_cv,msc,qmsc)
colnames(Tmsc) <- c("food_cpi","tot_cpi","food_cpi_lag","tot_cpi_lag","food_cpi_cv","tot_cpi_cv","msc","q_inflation")
Tsce <- ts.union(us_food_sce,us_tot_sce,us_food_sce_lag,us_tot_sce_lag,us_food_sce_cv,us_tot_sce_cv,sce,qsce)
colnames(Tsce) <- c("food_cpi","tot_cpi","food_cpi_lag","tot_cpi_lag","food_cpi_cv","tot_cpi_cv","sce","q_inflation")
Tbop <- ts.union(germany_food,germany_tot,germany_food_lag,germany_tot_lag,germany_food_cv,germany_tot_cv,bop,qbop)
colnames(Tbop) <- c("food_cpi","tot_cpi","food_cpi_lag","tot_cpi_lag","food_cpi_cv","tot_cpi_cv","bop","q_inflation")
if(mo!="sd"){
  Teuro <- ts.union(euro_food,euro_tot,euro_food_lag,euro_tot_lag,euro_food_cv,euro_tot_cv,ecfin,qecfin)
  colnames(Teuro) <- c("food_cpi","tot_cpi","food_cpi_lag","tot_cpi_lag","food_cpi_cv","tot_cpi_cv","ecfin","q_inflation")
}


## -- Run static time series

Tbop <- na.remove(Tbop)
bq <- lm(bop ~ q_inflation + food_cpi + tot_cpi, data = Tbop, na.action=na.exclude)
b <- lm(bop ~ food_cpi + tot_cpi, data = Tbop, na.action=na.exclude)
bql <- lm(bop ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tbop, na.action=na.exclude)
bl <- lm(bop ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tbop, na.action=na.exclude)
bqv <- lm(bop ~ q_inflation + food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Tbop, na.action=na.exclude)
bv <- lm(bop ~ food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Tbop, na.action=na.exclude)
bqlv <- lm(bop ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tbop, na.action=na.exclude)
blv <- lm(bop ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tbop, na.action=na.exclude)
summary(b)

if(mo!="sd"){
  is.na(Teuro) <- sapply(Teuro, is.infinite)
  Teuro <- na.remove(Teuro)
  eq <- lm(ecfin ~ q_inflation + food_cpi + tot_cpi , data = Teuro, na.action=na.exclude)
  e <- lm(ecfin ~ food_cpi + tot_cpi, data = Teuro, na.action=na.exclude)
  eql <- lm(ecfin ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Teuro, na.action=na.exclude)
  el <- lm(ecfin ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Teuro, na.action=na.exclude)
  eqv <- lm(ecfin ~ q_inflation + food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Teuro, na.action=na.exclude)
  ev <- lm(ecfin ~ food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Teuro, na.action=na.exclude)
  eqlv <- lm(ecfin ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Teuro, na.action=na.exclude)
  elv <- lm(ecfin ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Teuro, na.action=na.exclude)
  
  summary(e)
}

Tmsc <- na.remove(Tmsc)
mq <- lm(msc ~ q_inflation + food_cpi + tot_cpi, data = Tmsc, na.action=na.exclude)
m <- lm(msc ~ food_cpi + tot_cpi, data = Tmsc, na.action=na.exclude)
mql <- lm(msc ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tmsc, na.action=na.exclude)
ml <- lm(msc ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tmsc, na.action=na.exclude)
mqv <- lm(msc ~ q_inflation + food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Tmsc, na.action=na.exclude)
mv <- lm(msc ~ food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Tmsc, na.action=na.exclude)
mqlv <- lm(msc ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tmsc, na.action=na.exclude)
mlv <- lm(msc ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tmsc, na.action=na.exclude)

summary(m)

# Robustness: sub period
Tmscsub <- window(Tmsc, start = 2004)
msq <- lm(msc ~ q_inflation + food_cpi + tot_cpi , data = Tmscsub, na.action=na.exclude)
ms <- lm(msc ~ food_cpi + tot_cpi, data = Tmscsub, na.action=na.exclude)
msql <- lm(msc ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tmscsub, na.action=na.exclude)
msl <- lm(msc ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tmscsub, na.action=na.exclude)
msqv <- lm(msc ~ q_inflation + food_cpi + tot_cpi  + food_cpi_cv + tot_cpi_cv, data = Tmscsub, na.action=na.exclude)
msv <- lm(msc ~ food_cpi + tot_cpi  + food_cpi_cv + tot_cpi_cv, data = Tmscsub, na.action=na.exclude)
msqlv <- lm(msc ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag  + food_cpi_cv + tot_cpi_cv, data = Tmscsub, na.action=na.exclude)
mslv <- lm(msc ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag  + food_cpi_cv + tot_cpi_cv, data = Tmscsub, na.action=na.exclude)



Tsce <- na.remove(Tsce)
sq <- lm(sce ~ q_inflation + food_cpi + tot_cpi , data = Tsce, na.action=na.exclude)
s <- lm(sce ~ food_cpi + tot_cpi, data = Tsce, na.action=na.exclude)
sql <- lm(sce ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tsce, na.action=na.exclude)
sl <- lm(sce ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag, data = Tsce, na.action=na.exclude)
sqv <- lm(sce ~ q_inflation + food_cpi + tot_cpi  + food_cpi_cv + tot_cpi_cv, data = Tsce, na.action=na.exclude)
sv <- lm(sce ~ food_cpi + tot_cpi + food_cpi_cv + tot_cpi_cv, data = Tsce, na.action=na.exclude)
sqlv <- lm(sce ~ q_inflation + food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tsce, na.action=na.exclude)
slv <- lm(sce ~ food_cpi + tot_cpi + food_cpi_lag + tot_cpi_lag + food_cpi_cv + tot_cpi_cv, data = Tsce, na.action=na.exclude)

summary(s)

# --- Print output

# settings for stargazer
column.labels <- c("MSC","SCE","ECFIN","BOP")
title <- "Timeseries regression"
label <- paste("tab:timeseries_",mo,sep = "")

if(mo!="sd"){
  writeLines(capture.output(stargazer(m, mq, mql, msql, s, sq, sql, e, eq, eql, b, bq, bql,
                                    title = title, label = label, 
                                    column.labels = column.labels,  model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE)),
           file.path(outline, paste('code_tsreg_',mo,'.tex',sep = "")))
} else {
  writeLines(capture.output(stargazer(m, mq, mql,  msql, s, sq, sql, b, bq, bql,
                                      title = title, label = label, 
                                      column.labels = column.labels,  model.names = FALSE, 
                                      align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                      intercept.top = TRUE, intercept.bottom = FALSE)), 
            file.path(outline, paste('code_tsreg_',mo,'.tex',sep = "")))
}

# BOP
# settings for stargazer
title <- "Timeseries regression BOP"
label <- "tab:timeseries_bop"

writeLines(capture.output(stargazer(b, bq, bl, bv, bql, bqv, blv, bqlv,
                                      title = title, label = label, 
                                      model.names = FALSE, 
                                      align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                      intercept.top = TRUE, intercept.bottom = FALSE)),
             file.path(outline, 'code_tsreg_bop.tex'))

# MSC
# settings for stargazer
title <- "Timeseries regression MSC"
label <- "tab:timeseries_msc"

writeLines(capture.output(stargazer(m, mq, ml, mv, mql, mqv, mlv, mqlv,
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE)),
           file.path(outline, 'code_tsreg_msc.tex'))

# MSC sub
# settings for stargazer
title <- "Timeseries regression MSC subperiod"
label <- "tab:timeseries_mscs"

writeLines(capture.output(stargazer(ms, msq, msl, msv, msql, msqv, mslv, msqlv,
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE)),
           file.path(outline, 'code_tsreg_mscs.tex'))

# SCE
# settings for stargazer
title <- "Timeseries regression SCE"
label <- "tab:timeseries_sce"

writeLines(capture.output(stargazer(s, sq, sl, sv, sql, sqv, slv, sqlv,
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE)),
           file.path(outline, 'code_tsreg_sce.tex'))

# ECFIN
# settings for stargazer
title <- "Timeseries regression ECFIN"
label <- "tab:timeseries_ecfin"

writeLines(capture.output(stargazer(e, eq, el, ev, eql, eqv, elv, eqlv,
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE)),
           file.path(outline, 'code_tsreg_ecfin.tex'))



library(multcomp)
# m, mq, mql, msql, s, sq, sql, e, eq, eql, b, bq, bql,

summary(glht(m, "food_cpi - tot_cpi=0"))

## Leftover Code
################

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
