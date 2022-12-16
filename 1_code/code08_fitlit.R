# ------------
# Introduction
# ------------

## This file fits a financial literacy test score and subjective measure by 
## regressing within selected sample and
## predicting out of sample


rm(list=ls())
NAME <- 'code08_fitlit' ## Name of the R file goes here (without the file extension!)
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
library(plm)
library(psychTools) # for writing latex
library(stargazer) # for writing regression tables
library(pscl) # for McFadden R2
library(caret) # for variable importance
library(InformationValue) # for optimal cutoff
library(rms) # for ordered logit


## --------
## Settings
## --------
### Any settings go here



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


# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

T_fin <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out','base', 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out','base', 'T.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))


finlitnames <- c("prob_intqr", "nround", "refresher", "f_easy", "f_nointerest")
waves <- colnames(T_fin) %>%
  str_subset("w\\d")

## -- Regress actual measures to apply out of sample


f_subj <- as.formula(paste('fin_lit_subj ~ ', paste(finlitnames, collapse='+'),'+',paste(waves, collapse='+')))
f_test <- as.formula(paste('fin_lit_test ~ ', paste(finlitnames, collapse='+'),'+',paste(waves, collapse='+')))

options(datadist='ddist')
ddist<- datadist(finlitnames)
ddist<- datadist(waves)

lsubj <- lrm(f_subj, data= T_fin)
ltest <- lrm(f_test, data= T_fin)

# --- Comparing models


# settings for stargazer
title <- "Explaining financial literacy through financial confidence variables"
label <- "tab:fitlit"
dep.var.labels <- c("Subjective financial literacy","Financial literacy test score")

writeLines(capture.output(stargazer(lsubj, ltest, 
                                    title = title, label = label, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path('empirical','3_output','results', NAME,'code_fitlit.tex'))


## -- Fit variables

#calculate probability of default for each individual in test dataset
T$predicted_subj <- predict(lsubj, T)
T$predicted_test <- predict(ltest, T)


save(T, file = file.path(pipeline, 'out', 'T.RData'))