# ------------
# Introduction
# ------------

## This file uses the predicted female dummies instead of the actual to see how much of the female variable we can explain 

rm(list=ls())
NAME <- 'code08_usepredfem' ## Name of the R file goes here (without the file extension!)
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

if (!dir.exists(file.path(pipeline,'out',l))) {
  dir.create(file.path(pipeline,'out',l))
}

### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
}

if (!dir.exists(file.path('empirical', '3_output','results',NAME,l))) {
  dir.create(file.path('empirical', '3_output','results',NAME,l))
}

# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

load(file.path('empirical', '2_pipeline', 'code07_logfemale','out', 'T.RData'))

waves <- colnames(T) %>%
  str_subset("w\\d")

fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy')
hhroles <- c('shop_groceries','shop_major','prep_meals','decide_finance','live_alone')

xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','female','sentfemale','expfemale','finfemale')) %>%
  # need to also remove all financial confidence variables
  setdiff(fincon) %>%
  # need to remove hhroles
  setdiff(hhroles) %>%
  # need to remove pessimist
  setdiff('pessimist')

xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)

## -- define dependent variables

# in log
if (l == 'log') {
  T$y = log(T$y-min(T$y)+1)
}
# without log it's just y

# --- Include time varying as averages to control 

T_mean <- degroup(
  T,
  c(xtvnames),
  "id",
  center = "mean",
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  add_attributes = TRUE,
  verbose = TRUE
)

T_c <- cbind(T,T_mean) %>%
  pdata.frame(index=c( "id", "wave" ) )

# --- Fit regression specifications

# baseline with normal female
f <- as.formula(paste('y ~','factor(wave) + female +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
mbase <- plm( f, data=T_c, effect = "individual", model = "pooling")

# using female predicted by experience
f <- as.formula(paste('y ~','factor(wave) + expfemale +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
mexp <- plm( f, data=T_c, effect = "individual", model = "pooling")

# using female predicted by sentiment
f <- as.formula(paste('y ~','factor(wave) + sentfemale +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
msent <- plm( f, data=T_c, effect = "individual", model = "pooling")

# using female predicted by confidence
f <- as.formula(paste('y ~','factor(wave) + finfemale +', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+')))
mfin <- plm( f, data=T_c, effect = "individual", model = "pooling")


## --- Printing output


# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","_","between")
omit.labels <- c("Time dummies","Controls","Between effects")
title <- "Controlling variation in female"
label <- "tab:usepredfem"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# in which order
desiredOrder <- c("Constant","female","expfemale","sentfemale","finfemale","eduschool","eduwork","hhchildren","hhinc","pinc","age","citysize","eastgerman","east1989","part_time","unemployed","retired")

writeLines(capture.output(stargazer(mbase,mexp,msent,mfin, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path('empirical', '3_output','results', NAME,l, 'code_usepredfem.tex'))
