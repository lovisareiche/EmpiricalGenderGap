# ------------
# Introduction
# ------------

## Compares timeseries expectations from different surveys

rm(list=ls())
NAME <- 'code07_quantreg' ## Name of the R file goes here (without the file extension!)
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
library('datawizard')
library(stargazer)
library(quantreg)
library('plm')
library(zoo)
library(plm)


## --------
## Settings
## --------
### Any settings go here

S <- c('BOP-HH','Michigan','FRBNY')
# BOP-HH, Michigan, FRBNY - all you have

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

for(i in 1:length(S)){
  T <- read_csv(file.path('empirical', '2_pipeline',f, 'code01_align','out',S[i], 'T.csv')) %>%
    mutate(survey = S[i], date = as.yearmon(paste(year, month), format = "%Y %m"))
  assign(paste("T_",S[i],sep = ""),T)
  if(i==1){
    F <- T
  }
  if(i>1){
    F <- rbind(F,T)
  }
  
  # define names
  xnames <- colnames(T) %>%
    setdiff('year') %>%
    setdiff('month') %>%
    setdiff('y') %>%
    setdiff(c('survey','id','date'))
  xtinames <- c("female","region")
  xtvnames <- setdiff(xnames,xtinames)
  
  # bind between effects
  T_mean <- degroup(
    T,
    xtvnames,
    "id",
    center = "mean",
    suffix_demean = "_within",
    suffix_groupmean = "_between",
    add_attributes = TRUE,
    verbose = TRUE
  )
  
  T_c <- cbind(T,T_mean) %>%
    pdata.frame(index=c( "id", "date"))
  
  # formula
  form <- as.formula(paste('y ~','factor(year) +', paste(xnames, collapse='+')))
  
  # run quantile regression for range of taus
  qreg <- rq(form, data = T_c, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  assign(paste("qreg_",S[i],sep = ""),qreg)
  
  # run baseline to compute R2
  qreg0 <- rq(y ~ 1, data = T_c, tau = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
  rho <- function(u,tau=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))u*(tau - (u < 0))
  R2 <- 1 - qreg$rho/qreg0$rho
  assign(paste("R2_",S[i],sep = ""),R2)
  
  # s.qreg <- summary.rqs(qreg)
  # assign(paste("sqreg_",S[i],sep = ""),s.qreg)
  # 
  # jpeg(file.path(outline,paste("qreg_female_",S[i],".jpg",sep = "")), width = 1000, height = 700)
  # plot.summary.rqs(s.qreg, parm = "female", ols = TRUE)
  # dev.off()
  
  save(qreg, R2,  file = paste(pipeline,"/out/qreg_",S[i],".R.Data",sep = ""))
  
}

rm(T)

## Load BOP HH

load(paste(pipeline,"/out/qreg_","BOP-HH",".R.Data",sep = ""))

fem <- qreg[["coefficients"]]["female",]
F <- coefficients(qreg)
summary.rqs(qreg, se="boot")

