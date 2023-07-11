# ------------
# Introduction
# ------------

## This file predicts the binary indicators hhcluster, pessimist and subj fin literacy from the demographics.
## It shows that female is the biggest driver of both pessimism and fin lit

rm(list=ls())
NAME <- 'code10_predictlog' ## Name of the R file goes here (without the file extension!)
PROJECT <- 'EmpiricalGenderGap'
PROJECT_DIR <- 'D:/Lovisa/Studium/Oxford/Department of Economics/DPhil' ## Change this to the directory in which your project folder is located, make sure to avoid using single backslashes (i.e. \)!

# ------------
# Preamble
# ------------

## -------
## Imports
## -------
### All the library imports go here

library('plm')
library('tidyverse')
library('datawizard')
library(stargazer)
library(pscl) # for McFadden R2
library(caret) # for variable importance
library(InformationValue) # for optimal cutoff


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

if (!dir.exists(file.path('empirical', '3_output','results',f,NAME))) {
  outline <- file.path('empirical', '3_output','results',f,NAME)
  dir.create(outline)
}

# ---------
# Main code
# ---------

## -- Load data from pipeline folder --

load(file.path('empirical', '2_pipeline', f,'code09_fitlit','out', 'T.RData'))
hhcluster <- read_csv(file.path('empirical', '2_pipeline',f, 'cluster.m','out', 'hhcluster.csv'))
T["hhcluster"] <- hhcluster
T["pessimist"] <- as.numeric(T$pessimist>=3)


waves <- colnames(T) %>%
  str_subset("w\\d")

fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy','pred_subj_bin','pred_test_bin','pred_subj','pred_test')
hhroles <- c('shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','hhcluster')
xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','full_time_fem')) %>%
  # need to also remove all financial confidence variables
  setdiff(fincon) %>%
  # need to remove hhroles
  setdiff(hhroles) %>%
  # need to remove also pessimist
  setdiff(c('pessimist'))


## -- Run different specifications

# --- Set training and testing dataset

sample <- sample(c(TRUE, FALSE), nrow(T), replace=TRUE, prob=c(0.7,0.3))
train <- T[sample, ]
test <- T[!sample, ]

# --- Fit regression on training data


#fit logistic regression model for hhcluster binary variable
f <- as.formula(paste('hhcluster ~', paste(xnames, collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
mhh <- glm(f, family="binomial", data=train)

#fit logistic regression model for pessimist binary variable
f <- as.formula(paste('pessimist ~', paste(xnames, collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
mpess <- glm(f, family="binomial", data=train)

#fit logistic regression model for pred_subj_bin binary variable
f <- as.formula(paste('pred_subj_bin ~', paste(xnames, collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
mfin <- glm(f, family="binomial", data=train)



#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(mhh)
summary(mpess)
summary(mfin)

# --- Assessing model fit
mfr2hh <- pscl::pR2(mhh)["McFadden"]
mfr2pess <- pscl::pR2(mpess)["McFadden"]
mfr2fin <- pscl::pR2(mfin)["McFadden"]

# --- Variable importance

# contribution of variable
varimp <- caret::varImp(mhh)
myvalues <- varimp$Overall
mynames <- rownames(varimp)
varimphh <- setNames(myvalues, mynames)

varimp <- caret::varImp(mpess)
myvalues <- varimp$Overall
mynames <- rownames(varimp)
varimppess <- setNames(myvalues, mynames)

varimp <- caret::varImp(mfin)
myvalues <- varimp$Overall
mynames <- rownames(varimp)
varimpfin <- setNames(myvalues, mynames)

# check multicollinearity
varvifhh <- car::vif(mhh)
varvifpess <- car::vif(mpess)
varviffin <- car::vif(mfin)

# --- Use the model to make predictions

#calculate probability of default for each individual in test dataset
predhh <- predict(mhh, test, type="response")
predpess <- predict(mpess, test, type="response")
predfin <- predict(mfin, test, type="response")

#find optimal cutoff probability to use to maximize accuracy
opthh <- optimalCutoff(test$hhcluster, predhh)[1]
optpess <- optimalCutoff(test$pessimist, predpess)[1]
optfin <- optimalCutoff(test$pred_subj_bin, predfin)[1]

#calculate sensitivity
senhh <- sensitivity(test$hhcluster, predhh)
senpess <- sensitivity(test$pessimist, predpess)
senfin <- sensitivity(test$pred_subj_bin, predfin)


#calculate specificity
spehh <- specificity(test$hhcluster, predhh)
spepess <- specificity(test$pessimist, predpess)
spefin <- specificity(test$pred_subj_bin, predfin)


#calculate total misclassification error rate
misehh <- misClassError(test$hhcluster, predhh, threshold=opthh)
misepess <- misClassError(test$pessimist, predpess, threshold=optpess)
misefin <- misClassError(test$pred_subj_bin, predfin, threshold=optfin)


# --- Print


# settings for stargazer
notes <- paste("The full set of estimators included can be found in the appendix.")
omit <- c("q\\_|exp","si\\_","w\\d")
omit.labels <- c("Qualitative assessment","Shop intent","Time dummies")
title <- "Predicting binary indicators"
label <- "tab:predictlog"
r2 <- c("Mc Fadden $R^2$",round(mfr2hh, digits = 2),round(mfr2pess, digits = 2),round(mfr2fin, digits = 2))
mise <- c("Missclassification Error Rate",round(misehh, digits = 2),round(misepess, digits = 2),round(misefin, digits = 2))
sen <- c("Sensitivity",round(senhh, digits = 2),round(senpess, digits = 2),round(senfin, digits = 2))
spe <- c("Specificity",round(spehh, digits = 2),round(spepess, digits = 2),round(spefin, digits = 2))
add.lines <- list(r2,mise,sen,spe)


writeLines(capture.output(stargazer(mhh, mpess, mfin,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    intercept.top = TRUE, intercept.bottom = FALSE, 
                                    no.space = FALSE, add.lines = add.lines
                                    )), 
           file.path(outline,'code_predictlog.tex'))



