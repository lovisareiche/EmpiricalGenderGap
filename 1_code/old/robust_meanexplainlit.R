# ------------
# Introduction
# ------------

## Run an ordinal logistic regression to see how much of the financial 
## iteracy (subjective or test score) is indeed caused by the time 
## invariant and time variant variables. The output is a table that compares
## a model including and excluding time invariant variables in a cross-sectional 
## dataset (question was asked in waves 21 (subjective) and 25 (test)).

rm(list=ls())
NAME <- 'robust_meanexplainlit' ## Name of the R file goes here (without the file extension!)
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
library(pscl) # for McFadden R2
library(caret) # for variable importance
library(InformationValue) # for optimal cutoff
library(rms) # for ordered logit


## --------
## Settings
## --------
### Any settings go here

t <- 'base'

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

### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
}

if (!dir.exists(file.path('empirical', '3_output','results',NAME,t))) {
  dir.create(file.path('empirical', '3_output','results',NAME,t))
}


# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out',t, 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))

xnames <- setdiff(colnames(T),'id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff('fin_lit_subj') %>%
  setdiff('full_time') %>%
  setdiff('fin_lit_test')
xtinames <- c("eduschool","citysize","female","eastgerman","east1989","leave","homemaker","civil_servant","entrepreneur","eduschool_fem","citysize_fem","female","eastgerman_fem","east1989_fem","leave_fem","homemaker_fem","civil_servant_fem","entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)


# --- Set training and testing dataset

sample <- sample(c(TRUE, FALSE), nrow(T), replace=TRUE, prob=c(0.8,0.2))
train <- T[sample, ]
test <- T[!sample, ]

# --- Fit ordered regression on training data

#T$fin_lit_subj <- as.factor(T$fin_lit_subj)

f_subj <- as.formula(paste('fin_lit_subj ~ ', paste(xtvnames, collapse='+')))
f_test <- as.formula(paste('fin_lit_test ~ ', paste(xtvnames, collapse='+')))


ddist<- datadist(xtvnames)
options(datadist='ddist')

lsubjtv <- lrm(f_subj, data=T)
ltesttv <- lrm(f_test, data=T)

f_subj <- as.formula(paste('fin_lit_subj ~ ', paste(xnames, collapse='+')))
f_test <- as.formula(paste('fin_lit_test ~ ', paste(xnames, collapse='+')))


ddist<- datadist(xnames)
options(datadist='ddist')

lsubj <- lrm(f_subj, data=T)
ltest <- lrm(f_test, data=T)



# --- Comparing models


# settings for stargazer
notes <- "The full set of estimators included can be found in the appendix."
omit <- c("w\\d","y>=","si\\_")
omit.labels <- c("Time dummies","Constants","Shop intent")
title <- "Eplaining financial literacy through time variant and invariant variables"
label <- "tab:meanexplainlit"
dep.var.labels <- c("Subjective financial literacy","Financial literacy test score")

# which variables are removed
rmvars <- c(str_subset(names(coef(lsubj)),"w\\d"),str_subset(names(coef(lsubj)),"y>="),str_subset(names(coef(lsubj)),"si\\_"))
# which are staying
stayvars <- setdiff(names(coef(lsubj)),rmvars)
# in which order
desiredOrder <- c("female","live_alone","live_alone_fem",
                  "shop_groceries","shop_major","prep_meals",
                  "decide_finance","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest", "f_easy","eduschool","eduwork",
                  "hhchildren","hhinc","pinc","age","citysize",
                  "eastgerman","east1989","part_time","unemployed","retired")

writeLines(capture.output(stargazer(lsubj, lsubjtv, ltest, ltesttv,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = TRUE)), 
           file.path('empirical','3_output','results', NAME,t,'code_meanexplainlit.tex'))



# ----------
# Leftovers
# ----------
## Here you leave any code snippets or temporary code that you don't need but don't want to delete just yet



# --- Use the model to make predictions

# #calculate probability of default for each individual in test dataset
# predicted_subj <- predict(lsubj, test)
# predicted_test <- predict(ltest, test)
# 
# #find optimal cutoff probability to use to maximize accuracy
# optimal_subj <- optimalCutoff(test$fin_lit_subj, predicted_subj)
# optimal_test <- optimalCutoff(test$fin_lit_test, predicted_test)
# 
# #calculate sensitivity
# sen_subj <- sensitivity(test$fin_lit_subj, predicted_subj)
# sen_test <- sensitivity(test$fin_lit_test, predicted_test)
# 
# 
# #calculate specificity
# spe_subj <- specificity(test$fin_lit_subj, predicted_subj)
# spe_test <- specificity(test$fin_lit_test, predicted_test)
# 
# 
# #calculate total misclassification error rate
# mise_subj <- misClassError(test$fin_lit_subj, predicted_subj, threshold=optimal_subj)
# mise_test <- misClassError(test$fin_lit_test, predicted_test, threshold=optimal_test)
# 
