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

### The code below will automatically create an output folder for this code file if it does not exist.

if (!dir.exists(file.path('empirical', '3_output','results',NAME))) {
  dir.create(file.path('empirical', '3_output','results',NAME))
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

fincon <- c('prob_intqr','nround','refresher','f_nointerest','f_easy')
hhroles <- c('shop_groceries_nsing','shop_major_nsing','prep_meals_nsing','decide_finance_nsing','non_single')

xnames <- setdiff(colnames(T),waves) %>%
  setdiff('id') %>%
  setdiff('wave') %>%
  setdiff('y') %>%
  setdiff(c('full_time','female')) %>%
  # need to also remove all financial confidence variables
  setdiff(fincon) %>%
  # need to remove hhroles
  setdiff(hhroles) %>%
  # need to remove pessimist
  setdiff('pessimist')

xtinames <- c("eduschool","citysize","female","eastgerman","east1989",
              "leave","homemaker","civil_servant","entrepreneur",
              "eduschool_fem","citysize_fem","female","eastgerman_fem",
              "east1989_fem","leave_fem","homemaker_fem","civil_servant_fem",
              "entrepreneur_fem")
xtvnames <- setdiff(xnames,xtinames)

# Include time varying as averages to control 

T_mean <- degroup(
  T,
  c(xtvnames, fincon, hhroles, 'pessimist'),
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


# Full model
 
#f_control <- as.formula(paste('female ~', paste( c(xnames, fincon, hhroles, 'pessimist'), collapse='+'),'+',paste(paste( c(xtvnames, fincon, hhroles, 'pessimist'),"_between",sep = ""), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
f_time <- as.formula(paste('female ~', paste( c(xnames, fincon, hhroles, 'pessimist'), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
#f  <- as.formula(paste('female ~', paste( c(xnames, fincon, hhroles, 'pessimist'), collapse='+')))
fullm <- glm(f_time, family="binomial", data=train)

# Experience

f_time <- as.formula(paste('female ~', paste( c(xnames, hhroles), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
expm <- glm(f_time, family="binomial", data=train)

# Sentiment 

f_time <- as.formula(paste('female ~', paste( c(xnames, 'pessimist'), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
sentm <- glm(f_time, family="binomial", data=train)

# Financial Confidence

f_time <- as.formula(paste('female ~', paste( c(xnames, fincon), collapse='+'),'+',paste(waves[1:length(waves)-1], collapse='+')))
finm <- glm(f_time, family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(fullm)
summary(expm)
summary(sentm)
summary(finm)

# --- Assessing model fit
mfr2full <- pscl::pR2(fullm)["McFadden"]
mfr2exp <- pscl::pR2(expm)["McFadden"]
mfr2sent <- pscl::pR2(sentm)["McFadden"]
mfr2fin <- pscl::pR2(finm)["McFadden"]

# --- Variable importance

# contribution of variable
varimp <- caret::varImp(fullm)
myvalues <- varimp$Overall
mynames <- rownames(varimp)
varimp <- setNames(myvalues, mynames)

# check multicollinearity
varvif <- car::vif(fullm)

# --- Use the model to make predictions

#calculate probability of default for each individual in test dataset
predictedfull <- predict(fullm, test, type="response")
predictedsent <- predict(sentm, test, type="response")
predictedexp <- predict(expm, test, type="response")
predictedfin <- predict(finm, test, type="response")

# fit "female" in full sample
T$sentfemale <- predict(sentm, T_c, type="response")
T$expfemale <- predict(expm, T_c, type="response")
T$finfemale <- predict(finm, T_c, type="response")

#find optimal cutoff probability to use to maximize accuracy
optimalfull <- optimalCutoff(test$female, predictedfull)[1]
optimalexp <- optimalCutoff(test$female, predictedexp)[1]
optimalsent <- optimalCutoff(test$female, predictedsent)[1]
optimalfin <- optimalCutoff(test$female, predictedfin)[1]

#calculate sensitivity
senfull <- sensitivity(test$female, predictedfull)
senexp <- sensitivity(test$female, predictedexp)
sensent <- sensitivity(test$female, predictedsent)
senfin <- sensitivity(test$female, predictedfin)


#calculate specificity
spefull <- specificity(test$female, predictedfull)
spexp <- specificity(test$female, predictedexp)
spesent <- specificity(test$female, predictedsent)
spefin <- specificity(test$female, predictedfin)


#calculate total misclassification error rate
misefull <- misClassError(test$female, predictedfull, threshold=optimalfull)
miseexp <- misClassError(test$female, predictedexp, threshold=optimalexp)
misesent <- misClassError(test$female, predictedsent, threshold=optimalsent)
misefin <- misClassError(test$female, predictedfin, threshold=optimalfin)


# --- Comparing models

# create "models" that show variable importance (absolute value of z startstic) and VIF (variance inflation factor)
mvarimp <- fullm
mvarimp$coefficients <- varimp
mvarvif <- fullm
mvarvif$coefficients <- varvif

# settings for stargazer
notes <- paste("The full set of estimators included can be found in the appendix.")
omit <- c("q\\_|exp","si\\_","w\\d")
omit.labels <- c("Qualitative Macro","Shop intent","Time dummies")
title <- "Comparing model specifications"
label <- "tab:logfem"
dep.var.labels <- "Female"
column.labels <- c("Absolute z score", "Variance Inflation Factor","Regression Coefficients")
column.separate <- c(1,1,4)
se <- c(NA, NA, NULL, NULL, NULL, NULL)
tstat <- c(NA, NA, NULL, NULL, NULL, NULL)
p <- c(NA, NA, NULL, NULL, NULL, NULL)
r2 <- c("Mc Fadden $R^2$","","",round(mfr2full, digits = 2),round(mfr2exp, digits = 2),round(mfr2sent, digits = 2),round(mfr2fin, digits = 2))
mise <- c("Missclassification Error Rate","","",round(misefull, digits = 2),round(miseexp, digits = 2),round(misesent, digits = 2),round(misefin, digits = 2))
sen <- c("Sensitivity","","",round(senfull, digits = 2),round(senexp, digits = 2),round(sensent, digits = 2),round(senfin, digits = 2))
spe <- c("Specificity","","",round(spefull, digits = 2),round(spexp, digits = 2),round(spesent, digits = 2),round(spefin, digits = 2))
add.lines <- list(r2,mise,sen,spe)

# in which order
desiredOrder <- c("Constant","non_single","non_single_fem",
                  "shop_groceries_nsing","shop_major_nsing","prep_meals_nsing",
                  "decide_finance_nsing","pessimist","prob_intqr","refresher",
                  "nround","f_nointerest", "f_easy","eduschool","eduwork",
                  "hhchildren","hhinc","pinc","age","citysize",
                  "eastgerman","east1989","part_time","unemployed","retired")

writeLines(capture.output(stargazer(mvarimp, mvarvif, fullm, expm,sentm,finm,
                                    title = title, notes = notes, label = label, 
                                    omit = omit, omit.labels = omit.labels, 
                                    model.names = FALSE, 
                                    align=TRUE , df = FALSE, digits = 2, header = FALSE, 
                                    order = desiredOrder, intercept.top = TRUE, intercept.bottom = FALSE, 
                                    dep.var.labels = dep.var.labels, no.space = FALSE,
                                    p = p, se = se, t = tstat,
                                    add.lines = add.lines,
                                    column.labels = column.labels, column.separate = column.separate
                                    )), 
           file.path('empirical', '3_output','results', NAME, 'code_logfem.tex'))


## --- Save variables required

save(T, file = file.path(pipeline, 'out', 'T.RData'))

