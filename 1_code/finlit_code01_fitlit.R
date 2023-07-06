# ------------
# Introduction
# ------------

## This file fits a financial literacy test score by 
## regressing within selected sample and predicting out of sample


rm(list=ls())
NAME <- 'code01_fitlit' ## Name of the R file goes here (without the file extension!)
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
library(zoo) # for date vector
library(MASS) # for ordered logistic



## --------
## Settings
## --------
### Any settings go here
f <- 'finlit'
S <- c('BOP-HH','FRBNY')



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

## Add subfolders

for(s in 1:length(S)){
  if (!dir.exists(file.path(pipeline,'out',S[s]))) {
    dir.create(file.path(pipeline,'out',S[s]))
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

for(s in 1:length(S)){
  
  #load data
  T <- read_csv(file.path('empirical', '0_data', 'manual',S[s], 'T_fin.csv')) %>%
    mutate(date = as.yearmon(paste(year, month), format = "%Y %m")) %>%
    pdata.frame( index=c( "id", "date" ) )
  
  # Define variables used in regression
  if(S[s]=='BOP-HH'){
    finlitnames <- c("intqr", "round", "refresher", "qeasy", "qinterest")
  }
  if(S[s]=='FRBNY'){
    finlitnames <- c("intqr", "round", "refresher", "qinterest")
  }
  
  # define data used for predicting:
  Tsub <- na.omit(T)
  
  # Convert dependent variable to ordered factor
  Tsub$fin_lit_test <- ordered(Tsub$fin_lit_test, levels = c(0, 1, 2, 3),
                                     labels = c("Low", "Medium", "High", "Very High"))
  
  # formula
  eq <- as.formula(paste('fin_lit_test ~ age + female + single + hhinc + educ + region +', paste(finlitnames, collapse='+')))

  # ordinal logistic regression on sub sample for which we have the test
  lfin <- polr(eq, data= Tsub, method = "logistic",Hess=TRUE)
  # predict out of sample
  T$lfinpred <- predict(lfin, newdata = T, type="probs")[,4]   #gets Prob(>=3) 
  
  # Save T with new predicted variables
  write_csv(T, file.path(pipeline, 'out',S[s],'T.csv'))

assign(paste("T_",S[s],sep = ""),T)
assign(paste("lfin_",S[s],sep = ""),lfin)
}

# settings for stargazer
title <- "Explaining financial literacy through financial confidence variables"
label <- "tab:fitlit"
omit <- c("factor","_between")
omit.labels <- c("Time dummies","Between effects")
column.labels <- c("BOP-HH","SCE")


writeLines(capture.output(stargazer(`lfin_BOP-HH`,lfin_FRBNY,
                                    title = title, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    column.labels = column.labels, model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    no.space = FALSE)), 
           file.path(outline,'code_fitlit.tex'))


