# ------------
# Introduction
# ------------

## This file computes the regressions for the cleaned unbalanced dataset

rm(list=ls())
NAME <- 'robust_uncertaintydrivers' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

library('plm') # for panel regression
library('tidyverse')
library('datawizard') # for degrouping
library(psychTools) # for writing latex
library(stargazer) # for writing regression tables


## --------
## Settings
## --------
### Any settings go here

t <- 'base'
l <- 'level'


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

if (!dir.exists(file.path(pipeline,'out',t))) {
  dir.create(file.path(pipeline,'out',t))
}

if (!dir.exists(file.path(pipeline,'out',t,l))) {
  dir.create(file.path(pipeline,'out',t,l))
}


# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out',t, 'T.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))


## --- Settings

# define names

waves <- colnames(T) %>%
  str_subset("w\\d")
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff('full_time')

xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)

# Include time varying as averages to control 

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
  pdata.frame(index=c( "id", "wave" ) )


## --- Run regressions in full and with reduced subset

f <- as.formula(paste('y ~','factor(wave) +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.base <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,'prob_intqr'), collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.intqr <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,'refresher'), collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.refresh <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,'nround'), collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.nround <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,'f_nointerest'), collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.ninterest <- plm( f, data=T_c, effect = "individual", model = "pooling")

f <- as.formula(paste('y ~','factor(wave) +', paste(setdiff(xnames,'f_easy'), collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
y.easy <- plm( f, data=T_c, effect = "individual", model = "pooling")

## --- Stargazer

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","q\\_|exp","si\\_","between")
omit.labels <- c("Time dummies","Macro qualitative","Shop intent","Between effects")
title <- "Drivers of uncertainty effect"
label <- "tab:uncertaintydrivers"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# which variables are removed
rmvars <- c(str_subset(names(coef(y.base)),"wave"),str_subset(names(coef(y.base)),"q\\_|exp"),str_subset(names(coef(y.base)),"si\\_"),str_subset(names(coef(y.base)),"between"))
# which are staying
stayvars <- setdiff(names(coef(y.base)),rmvars)
# in which order
desiredOrder <- c("Constant","female","live_alone","shop_groceries","shop_major",
                  "prep_meals","decide_finance","pessimist","prob_intqr",
                  "refresher","nround","f_nointerest","f_easy","eduschool",
                  "eduwork","hhchildren","hhinc","pinc","age","citysize",
                  "eastgerman","east1989","part_time","unemployed",
                  "retired")

writeLines(capture.output(stargazer(y.base, y.intqr, y.refresh, y.nround, y.ninterest, y.easy, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path('empirical', '2_pipeline', NAME,'out',t,l, 'code_compest.tex'))

