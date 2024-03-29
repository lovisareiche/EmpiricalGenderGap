# ------------
# Introduction
# ------------

## Create a latex file to compare the models.
## Settings: choose estimator used (typically LSDV_control) and level or logs
## Output files: comparison of base, interaction, no pessimist, no uncertainty, no hhroles or none of them
##               comparison of no quali, no shopintent, no geo, no edu, no inc
## Note that some variables are not shown in final table. This needs to be specified in the code.

rm(list=ls())
NAME <- 'code06_compmod' ## Name of the R file goes here (without the file extension!)
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
library(psychTools) # for writing latex
library(stargazer) # for writing regression tables
library(miceadds) # for loading individual files

## --------
## Settings
## --------
### Any settings go here

t <- 'LSDVt_control'
l <- 'level'
f <- 'bopreg'


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

## Add subfolders

if (!dir.exists(file.path(pipeline,'out',t))) {
  dir.create(file.path(pipeline,'out',t))
}

if (!dir.exists(file.path(pipeline,'out',t,l))) {
  dir.create(file.path(pipeline,'out',t,l))
}


if (!dir.exists(file.path(outline,t))) {
  dir.create(file.path('empirical', '3_output','results',NAME,t))
}

if (!dir.exists(file.path(outline,t,l))) {
  dir.create(file.path('empirical', '3_output','results',NAME,t,l))
}


# ---------
# Functions
# ---------



extractorRData <- function(file, object) {
  # Function for extracting an object from a .RData file created by R's save() command
  # Inputs: RData file, object name
  E <- new.env()
  load(file=file, envir=E)
  return(get(object, envir=E, inherits=F))
}

# ---------
# Main code
# ---------



# --- Load different models from pipeline folder --  

base <- extractorRData(file.path('empirical', '2_pipeline', f,'code04_regress','out','base',l, 'T.RData'),paste('y.',t, sep = ""))
int <- extractorRData(file.path('empirical', '2_pipeline', f,'code04_regress','out','int',l, 'T.RData'),paste('y.',t, sep = ""))
#edu <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_edu',l, 'T.RData'),paste('y.',t, sep = ""))
#employ <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_employ',l, 'T.RData'),paste('y.',t, sep = ""))
#geo <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_geo',l, 'T.RData'),paste('y.',t, sep = ""))
hhroles <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_hhroles',l, 'T.RData'),paste('y.',t, sep = ""))
#quali <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_quali',l, 'T.RData'),paste('y.',t, sep = ""))
pessimist <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_pessimist',l, 'T.RData'),paste('y.',t, sep = ""))
#inc <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_inc',l, 'T.RData'),paste('y.',t, sep = ""))
#shopintent <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_shopintent',l, 'T.RData'),paste('y.',t, sep = ""))
uncertainty <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','no_uncertainty',l, 'T.RData'),paste('y.',t, sep = ""))
nocontrol  <- extractorRData(file.path('empirical', '2_pipeline',f, 'code04_regress','out','demo_only',l, 'T.RData'),paste('y.',t, sep = ""))

# --- Comparing models

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","between","q\\_|exp","si\\_")
omit.labels <- c("Time dummies","Between effects","Macro qualitative","Shop intent")
title <- "Comparing model specifications"
label <- "tab:compmod"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# which variables are removed
rmvars <- c(str_subset(names(coef(int)),"wave"),str_subset(names(coef(int)),"between"),str_subset(names(coef(int)),"q\\_|exp"),str_subset(names(coef(int)),"si\\_"),str_subset(names(coef(int)),"eduwork"))
# which are staying
stayvars <- setdiff(names(coef(int)),rmvars)
# in which order
desiredOrder <- c("Constant","female","non_single","non_single_fem",
                  "shop_groceries_nsing","shop_groceries_nsing_fem","shop_major_nsing",
                  "shop_major_nsing_fem","prep_meals_nsing","prep_meals_nsing_fem",
                  "decide_finance_nsing","decide_finance_nsing_fem","pessimist",
                  "pessimist_fem","prob_intqr","prob_intqr_fem","refresher",
                  "refresher_fem","nround","nround_fem","f_nointerest",
                  "f_nointerest_fem","f_easy","f_easy_fem","eduschool",
                  "eduschool_fem","hhchildren","hhchildren_fem","hhinc",
                  "hhinc_fem","pinc","pinc_fem","age","age_fem","citysize",
                  "citysize_fem","eastgerman","eastgerman_fem","east1989",
                  "east1989_fem","part_time",
                  "part_time_fem","unemployed","unemployed_fem","retired",
                  "retired_fem")

writeLines(capture.output(stargazer(base, int, pessimist, uncertainty, hhroles, nocontrol,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = TRUE)), 
           file.path(outline,t,l, 'code_compmod.tex'))


# label <- "tab:compmoddemo"
# writeLines(capture.output(stargazer(quali, shopintent, geo, edu, inc,
#                                     title = title, notes = notes, label = label, 
#                                     omit = omit, omit.labels = omit.labels, 
#                                     model.names = FALSE, 
#                                     align=TRUE , df = FALSE, digits = 2, header = FALSE, 
#                                     order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
#                                     dep.var.labels = dep.var.labels, no.space = TRUE)), 
#            file.path(outline,t,l, 'code_compmod_demo.tex'))
# 

