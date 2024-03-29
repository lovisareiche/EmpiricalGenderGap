# ------------
# Introduction
# ------------

## Create a latex file to compare the estimators.
## Settings: choose combination of vars (typically base) and level or logs
## Output files: summary statistics and comparison of y.OLS, y.PO, y.RE, y.FEt, y.LSDVt_control
## Note that some variables are not shown in final table. This needs to be specified in the code.
## The code runs Beusch Pagan test (null: homoskedasticity) and Hausman test for endogeneity (RE on subset)

rm(list=ls())
NAME <- 'code05_compest' ## Name of the R file goes here (without the file extension!)
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
library(lmtest)

## --------
## Settings
## --------
### Any settings go here

t <- 'base'
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
  dir.create(file.path(outline,t))
}

if (!dir.exists(file.path(outline,t,l))) {
  dir.create(file.path(outline,t,l))
}

# ---------
# Main code
# ---------


  
# --- Load data from pipeline folder --  
  
load(file.path('empirical', '2_pipeline', f,'code04_regress','out',t,l, 'T.RData'))

# --- Summary Statistics

writeLines(capture.output(stargazer(T,title="Summary Statistics",align=TRUE,
                                    label = "tab:summary", model.names = TRUE)), 
           file.path(outline,t,l, 'code_summary.tex'))

# --- Comparing estimators

# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("wave","q\\_|exp","si\\_","between")
omit.labels <- c("Time dummies","Macro qualitative","Shop intent","Between effects")
column.labels <- c("OLS","PO","RE","FE","LSDV\\_control")
title <- "Comparing estimators"
label <- "tab:compest"
dep.var.labels <- "Inflation expectation, 12 months ahead, point estimate"

# which variables are removed
rmvars <- c(str_subset(names(coef(y.LSDVt_control)),"wave"),str_subset(names(coef(y.LSDVt_control)),"q\\_|exp"),str_subset(names(coef(y.LSDVt_control)),"si\\_"),str_subset(names(coef(y.LSDVt_control)),"between"))
# which are staying
stayvars <- setdiff(names(coef(y.LSDVt_control)),rmvars)
# in which order
desiredOrder <- c("Constant","female","non_single","shop_groceries_nsing",
                  "shop_major_nsing","prep_meals_nsing","decide_finance_nsing",
                  "pessimist","prob_intqr","refresher","nround","f_nointerest",
                  "f_easy","eduschool","eduwork","hhchildren","hhinc","pinc",
                  "age","citysize","eastgerman","east1989","full_time","part_time",
                  "unemployed","retired")

writeLines(capture.output(stargazer(y.OLS, y.PO, y.RE, y.FEt, y.LSDVt_control, 
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    column.labels = column.labels,  model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels)), 
           file.path(outline,t,l, 'code_compest.tex'))

# --- Run tests

#perform Breusch-Pagan Test for Heteroskedasticity
# null: homoskedasticity, reject if p<0.05
bptest(y.OLS)

#perform Hausman Test for Endogeneity
# null: exogenous, they are the same, reject if p<0.05
phtest(y.FEt,y.RE_subset)

