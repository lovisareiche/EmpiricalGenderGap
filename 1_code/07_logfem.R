# ------------
# Introduction
# ------------

## This file computes the regressions for the cleaned unbalanced dataset

rm(list=ls())
NAME <- 'code07_logfemale' ## Name of the R file goes here (without the file extension!)
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

# ---------
# Functions
# ---------


# ---------
# Main code
# ---------


## -- Load data from pipeline folder --

T <- read_csv(file.path('empirical', '2_pipeline', 'code03_compilepanel.m','out',t, 'T.csv')) %>%
  pdata.frame(index=c( "id", "wave" ))

waves <- colnames(T) %>%
  str_subset("w\\d")
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff('female') %>%
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

# --- Set training and testing dataset

sample <- sample(c(TRUE, FALSE), nrow(T_c), replace=TRUE, prob=c(0.7,0.3))
train <- T_c[sample, ]
test <- T_c[!sample, ]

# --- Fit regression on training data


#fit logistic regression model
f_control <- as.formula(paste('female ~', paste(xnames, collapse='+'),'+',paste(paste(xtvnames,"_between",sep = ""), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
f_time <- as.formula(paste('female ~', paste(xnames, collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
f  <- as.formula(paste('female ~', paste(xnames, collapse='+')))
model <- glm(f_time, family="binomial", data=train)


#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

# --- Assessing model fit
mfr2 <- pscl::pR2(model)["McFadden"]

# --- Variable importance

# contribution of variable
varimp <- caret::varImp(model)
myvalues <- varimp$Overall
mynames <- rownames(varimp)
varimp <- setNames(myvalues, mynames)

# check multicollinearity
varvif <- car::vif(model)

# --- Use the model to make predictions

#calculate probability of default for each individual in test dataset
predicted <- predict(model, test, type="response")

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$female, predicted)[1]
optimal

#calculate sensitivity
sen <- sensitivity(test$female, predicted)


#calculate specificity
spe <- specificity(test$female, predicted)


#calculate total misclassification error rate
mise <- misClassError(test$female, predicted, threshold=optimal)


# --- Comparing models

# create "models" that show variable importance (absolute value of z startstic) and VIF (variance inflation factor)
modelvarimp <- model
modelvarimp$coefficients <- varimp
modelvarvif <- model
modelvarvif$coefficients <- varvif

# settings for stargazer
notes <- paste("The full set of estimators included can be found in the appendix. Model sensitivity:",sen,"Model secificity:",spe, sep = " ")
omit <- c("w\\d","si\\_")
omit.labels <- c("Time dummies","Shop intent")
title <- "Comparing model specifications"
label <- "tab:logfem"
dep.var.labels <- "Female"
column.labels <- c("Absolute z score", "Variance Inflation Factor","Estimates")
se <- c(NA, NA, NULL)
tstat <- c(NA, NA, NULL)
p <- c(NA, NA, NULL)

# which variables are removed
rmvars <- c(str_subset(names(coef(model)),"w\\d"),str_subset(names(coef(model)),"between"),str_subset(names(coef(model)),"si\\_"))
# which are staying
stayvars <- setdiff(names(coef(model)),rmvars)
# in which order
desiredOrder <- c("Constant","live_alone","live_alone_fem",
                  "shop_groceries","shop_major","prep_meals",
                  "decide_finance","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest", "f_easy","eduschool",
                  "hhchildren","hhinc","pinc","age","citysize",
                  "eastgerman","east1989","part_time","unemployed","retired")

writeLines(capture.output(stargazer(modelvarimp, modelvarvif, model,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE,
                                    p = p, se = se, t = tstat,
                                    column.labels = column.labels)), 
           file.path(pipeline,'out',t, 'code_logfem.tex'))


