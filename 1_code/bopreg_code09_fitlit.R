# ------------
# Introduction
# ------------

## This file fits a financial literacy test score and subjective measure by 
## regressing within selected sample and
## predicting out of sample


rm(list=ls())
NAME <- 'code09_fitlit' ## Name of the R file goes here (without the file extension!)
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

if (dir.exists(file.path('empirical', '3_output','results',f,NAME))){
  outline <- file.path('empirical', '3_output','results',f,NAME)
} else {
  outline <- file.path('3_output','results',f,NAME)
}

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  dir.create(outline)
}


# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

T_fin <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T_fin.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))

T <- read_csv(file.path('empirical', '2_pipeline',f, 'code03_compilepanel.m','out','base', 'T.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))


# Define variables used in regression and time dummies
finlitnames <- c("prob_intqr", "nround", "refresher", "f_easy", "f_nointerest")
waves_fin <- colnames(T_fin) %>%
  str_subset("w\\d")




### Ordinal Logistic model 
###############################

# formula
f_subj <- as.formula(paste('fin_lit_subj ~ ', paste(finlitnames, collapse='+')))
f_test <- as.formula(paste('fin_lit_test ~ ', paste(finlitnames, collapse='+')))

# Run Regression
lsubj <- lrm(f_subj, data= T_fin)
ltest <- lrm(f_test, data= T_fin)

# predict out of sample
T$lpred_test <- predict(ltest, newdata = T, type="fitted.ind")[,4]   #gets Prob(>=3) 
T$lpred_subj <- predict(lsubj, newdata = T, type="fitted.ind")[,4]   #gets Prob(>=3) 


### Poisson
###############################

# same formula

# regression
psubj <- glm(f_subj, family="poisson", data=T_fin)
ptest <- glm(f_test, family="poisson", data=T_fin)

# predict out of sample
T$ppred_subj <- predict(psubj, T, type ="response")
T$ppred_test <- predict(ptest, T, type ="response")



# --- Comparing models


# settings for stargazer
title <- "Explaining financial literacy through financial confidence variables"
label <- "tab:fitlit"
dep.var.labels <- c("Subjective financial literacy","Financial literacy test score")
omit <- c("w\\d","_between")
omit.labels <- c("Time dummies","Between effects")
column.labels <- c("Ordered logit","Poisson","Ordered logit","Poisson")


writeLines(capture.output(stargazer(lsubj, psubj, ltest, ptest,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    column.labels = column.labels, model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE)), 
           file.path(outline,'code_fitlit.tex'))


## -- Save T with new predicted variables
save(T, file = file.path(pipeline, 'out', 'T_fin.RData'))
