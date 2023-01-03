# ------------
# Introduction
# ------------

## This file fits a distribution to the data 

rm(list=ls())
NAME <- 'data_fitdist' ## Name of the R file goes here (without the file extension!)
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
library("viridis")  # colour
library(MASS) # fit distributions to data
library(brms) # for shifted log normal

## --------
## Settings
## --------
### Any settings go here

s <- "hhcluster"
d <- "lognormal"
# run "female", "pessimist", "subj_finilliterate","hhcluster"

## ---------------------
## Set working directory
## ---------------------
### The code below will traverse the path upwards until it finds the root folder of the project.

setwd(file.path(PROJECT_DIR, PROJECT))

## ----------------------------------
## Set  up pipeline folder if missing
## ----------------------------------
### The code below will automatically create a pipeline folder for this code file if it does not exist.

if (dir.exists(file.path('empirical', '2_pipeline'))){
  pipeline <- file.path('empirical', '2_pipeline', NAME)
} else {
  pipeline <- file.path('2_pipeline', NAME)
}

if (!dir.exists(pipeline)) {
  dir.create(pipeline)
  for (folder in c('out', 'store', 'tmp')){
    dir.create(file.path(pipeline, folder))
  }
}

### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
}

if (!dir.exists(file.path('empirical', '3_output','results',NAME,d))) {
  dir.create(file.path('empirical', '3_output','results',NAME,d))
}




# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

load(file.path('empirical', '2_pipeline', 'code09_fitlit','out', 'T.RData'))
hhcluster <- read_csv(file.path('empirical', '2_pipeline', 'cluster.m','out', 'hhcluster.csv'))
T["hhcluster"] <- hhcluster
T["pessimist"] <- as.numeric(T$pessimist>=3)
T["subj_finilliterate"] <- as.numeric(T$pred_subj_bin==0)


# Truncate data to avoid results being driven by outliers
T <- T[T$y>=-5,]
T <- T[T$y<=20,]

## -- define dependent variables

# min value
yshift <- -min(T$y)+0.1
# in log
lny = log(T$y+yshift)
# in level still needs to be shifted
y <- T$y+yshift


## -- Separate inflation expectations by s

y1 <- y[T[s]==0]
y2 <- y[T[s]==1]


# create a vector of histogram breaks
x <- seq(min(T$y),max(T$y),length=301)

# histogram the data
hst <- hist(y-yshift, breaks=x)


## -- fit log normal distribution

# fit a log normal distribution
fit_params1 <- fitdistr(y1,d)
fit_params2 <- fitdistr(y2,d)

# generate values given our fit parameters
fit1 <- dshifted_lnorm(x, meanlog = fit_params1$estimate["meanlog"], sdlog = fit_params1$estimate["sdlog"], shift = -yshift, log = FALSE)
fit2 <- dshifted_lnorm(x, meanlog = fit_params2$estimate["meanlog"], sdlog = fit_params2$estimate["sdlog"], shift = -yshift, log = FALSE)

mean1 = exp(fit_params1$estimate["meanlog"] + (fit_params1$estimate["sdlog"]^2)/2)-yshift
mean2 = exp(fit_params2$estimate["meanlog"] + (fit_params2$estimate["sdlog"]^2)/2)-yshift
mode1 = exp(fit_params1$estimate["meanlog"] - (fit_params1$estimate["sdlog"]))-yshift
mode2 = exp(fit_params2$estimate["meanlog"] - (fit_params2$estimate["sdlog"]))-yshift

# plot the fit and original distributions
jpeg(file.path('empirical','3_output','results', NAME,d,paste("density_",s,".jpg", sep = "")), width = 1000, height = 700)

plot(x, fit1, type="l", ylab="Density",
     xlab="Point estimate of inflation in 12 months", ylim=c(0,0.4), xlim=c(-5,20), lwd = 2,
     col = '#238a8DFF')
lines(x, fit2, type="l", ylab="Density",
     xlab="Point estimate of inflation in 12 months", ylim=c(0,0.4), xlim=c(-5,20), lwd = 2,
     col = '#FDE725FF')
abline(v=mean1, col='#238a8DFF')
abline(v=mean2, col ='#FDE725FF' )
title(main = paste("Density histogram with",d,"fit:" , s, sep = " "))
legend("topleft", c(paste("not",s, sep = " "),s), fill = c('#238a8DFF','#FDE725FF'))
dev.off()
